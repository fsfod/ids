// Copyright (c) 2021 Saleem Abdulrasool.  All Rights Reserved.
// SPDX-License-Identifier: BSD-3-Clause

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Edit/EditsReceiver.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "clang/Lex/PPCallbacks.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/GlobPattern.h"
#include "llvm/Support/Path.h"

#include <cstdlib>
#include <iostream>
#include <set>
#include <string>
#pragma hdrstop

namespace idt {
llvm::cl::OptionCategory category{"interface definition scanner options"};
}

namespace {

llvm::cl::opt<std::string>
export_macro("export-macro",
             llvm::cl::desc("The macro to decorate interfaces with"),
             llvm::cl::value_desc("define"), llvm::cl::Required,
             llvm::cl::cat(idt::category));

llvm::cl::opt<std::string>
function_export_macro("function-macro",
  llvm::cl::desc("The macro to decorate non class functions with"),
  llvm::cl::value_desc("define"), llvm::cl::Optional,
  llvm::cl::cat(idt::category));

llvm::cl::opt<std::string>
template_export_macro("template-macro",
  llvm::cl::desc("The macro to decorate non class functions with"),
  llvm::cl::value_desc("define"), llvm::cl::Optional,
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
apply_fixits("apply-fixits", llvm::cl::init(false),
             llvm::cl::desc("Apply suggested changes to decorate interfaces"),
             llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
inplace("inplace", llvm::cl::init(false),
        llvm::cl::desc("Apply suggested changes in-place"),
        llvm::cl::cat(idt::category));

llvm::cl::list<std::string>
ignored_functions("ignore",
                  llvm::cl::desc("Ignore one or more functions"),
                  llvm::cl::value_desc("function-name[,function-name...]"),
                  llvm::cl::CommaSeparated,
                  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
mainfileonly("mainfileonly", llvm::cl::init(false),
  llvm::cl::desc("Only apply fixits to main files specified on the command line, not included headers"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<std::string>
header_directory("headerdir",
  llvm::cl::desc("Directory to recursively search for headers to rewrite"),
  llvm::cl::value_desc("define"),
  llvm::cl::cat(idt::category));

llvm::cl::list<std::string>
ignored_headers("header-ignore",
  llvm::cl::desc("Ignore one or more header files"),
  llvm::cl::value_desc("header[,header...]"),
  llvm::cl::CommaSeparated,
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
export_extern_c("export-extern-c", llvm::cl::init(false),
  llvm::cl::desc("Add export macros to extern C declarations"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
skip_simple_structs("skip-simple-structs", llvm::cl::init(true),
  llvm::cl::desc("Skip exporting simple structs that "
    "don't have any out of line methods or"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
func_macro_on_name("function-macro-on-name", llvm::cl::init(false),
  llvm::cl::desc("Change the attach point for function export macros to next to there name"),
  llvm::cl::cat(idt::category));

template <typename Key, typename Compare, typename Allocator>
bool contains(const std::set<Key, Compare, Allocator>& set, const Key& key) {
  return set.find(key) != set.end();
}

const std::set<std::string> &get_ignored_functions() {
  static auto kIgnoredFunctions = [&]() -> std::set<std::string> {
      return { ignored_functions.begin(), ignored_functions.end() };
    }();

  return kIgnoredFunctions;
}

}

const llvm::SmallVector<llvm::GlobPattern> &get_ignored_headers() {
  static auto kIgnoredHeaders = [&]() -> auto {
    llvm::SmallVector<llvm::GlobPattern> headers;
    llvm::SmallString<256> root;
    llvm::sys::path::native(header_directory, root);

    for (const auto& P : ignored_headers) {
      auto Patten = llvm::GlobPattern::create(P);
      if (!Patten) {
        llvm::outs() << "Bad header path glob " << P << Patten.takeError();
      } else {
        headers.push_back(std::move(Patten.get()));
      }
    }
    return headers;
  }();

  return kIgnoredHeaders;
}


namespace idt {
class visitor : public clang::RecursiveASTVisitor<visitor> {
  clang::ASTContext &context_;
  clang::SourceManager &source_manager_;
  clang::Sema *sema;
  bool skip_function_bodies;

  clang::DiagnosticBuilder
  unexported_public_interface(clang::SourceLocation location) {
    clang::DiagnosticsEngine &diagnostics_engine = context_.getDiagnostics();

    unsigned kID =
        diagnostics_engine.getCustomDiagID(clang::DiagnosticsEngine::Remark,
                                           "unexported public interface %0");

    return diagnostics_engine.Report(location, kID);
  }

  clang::DiagnosticBuilder
  exported_private_interface(clang::SourceLocation location) {
    clang::DiagnosticsEngine &diagnostics_engine = context_.getDiagnostics();

    unsigned kID =
        diagnostics_engine.getCustomDiagID(clang::DiagnosticsEngine::Remark,
                                           "exported private interface %0");

    return diagnostics_engine.Report(location, kID);
  }

  template <typename Decl_>
  inline clang::FullSourceLoc get_location(const Decl_ *TD) const {
    return context_.getFullLoc(TD->getBeginLoc()).getExpansionLoc();
  }

public:
  explicit visitor(clang::ASTContext &context, bool skipFuncBodies)
      : context_(context), source_manager_(context.getSourceManager()), sema(nullptr), 
        skip_function_bodies(skipFuncBodies) {
  }

  bool debuglog = false;

  bool VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (D->isEnum())
      return true;

    // We won't visit some forward declared classes again that get a definition in same translation unit
    if (!D->isThisDeclarationADefinition()) {
      auto realDefinition = D->getDefinition();
      if (!realDefinition)
        return true;

      D = realDefinition;
    }

    clang::FullSourceLoc location = get_location(D);

    if (ShouldSkipDeclaration(D))
      return true;

    if (isAlreadyExported(D, true))
      return true;

    auto parent = D->getDeclContext();
    // Skip non templated class/structs in a templated type
    if (D->isDependentContext())
      return true;
    bool outOfLineMembers = false;
    const auto *CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);

    for (const clang::CXXMethodDecl* MD : D->methods()) {
      if (MD->isImplicit() || MD->isDeleted() || MD->isDefaulted() || MD->isPureVirtual())
        continue;

      if (!MD->hasBody()) {
        if ((CTSD || skip_function_bodies) && MD->isInlined()) {
          continue;
        }
        outOfLineMembers = true;
        break;
      }
    }

    bool staticFields = llvm::any_of(D->fields(), [](const clang::FieldDecl* FD) {
      if (const VarDecl *VD = dyn_cast<VarDecl>(FD)) {
        return VD->isStaticDataMember() && !VD->hasInit();
      }
      return false;
    });

    bool requiresExport = outOfLineMembers || staticFields;

    // Don't add DLL export to PoD structs that also have no methods
    if (D->isStruct() && skip_simple_structs && !requiresExport) {
      LogSkippedDecl(D, location, " that is SimpleStruct");
      return true;
    }

    // Check if this ia nested class or struct
    if (const RecordDecl *PD = dyn_cast<clang::RecordDecl>(parent)) {
      assert(PD->isClass() || PD->isStruct() || PD->isUnion());

      // Skip nested class and structs that have no methods or there all declared inside the declaration
      if (!requiresExport) {
        return true;
      }
    }

    auto templClass = D->getDescribedClassTemplate();
    if (templClass) {
      return true;
    }

    clang::SourceLocation insertion_point = D->getLocation();
    std::string exportMacro = export_macro;

    if (CTSD) {
      if (debuglog) {
        llvm::outs() << "TemplateDecl: " << CTSD->getNameAsString() << ", Body: " << CTSD->hasBody()
          << ", Definition: " << CTSD->hasDefinition() << ", Specialization Kind: "
          << CTSD->getSpecializationKind() << "\n";
      }

      // We only want export full class specialization
      if (llvm::isa<ClassTemplatePartialSpecializationDecl>(CTSD))
        return true;

      // Skip nested classes
      if (CTSD->isClassScopeExplicitSpecialization())
        return true;

      if (CTSD->getTemplateSpecializationKind() == TSK_ExplicitInstantiationDeclaration) {
        exportMacro = template_export_macro;
        if (!CTSD->getExternLoc().isValid())
          return true;

        if (!CTSD->hasExternalFormalLinkage()) {
          llvm::dbgs() << "NO external formal linkage\n";
          return true;
        }

        // TODO is there a better way todo this
        auto sourceText = Lexer::getSourceText(clang::CharSourceRange::getTokenRange(
                                                          CTSD->getExternLoc(), D->getLocation()), 
                                                          context_.getSourceManager(), context_.getLangOpts());
        // Don't apply the macro if it already has one
        if (sourceText.contains(exportMacro)) {
          return true;
        }

        size_t keywordOffset = sourceText.find(D->isStruct() ? "struct" : "class");

        if (keywordOffset == llvm::StringRef::npos) {
          llvm::errs() << "Failed to find class or struct keyword for extern template declaration " << sourceText;
          return true;
        }
        // Insert one space after the keyword
        keywordOffset += D->isStruct() ? 7 : 6;
        insertion_point = CTSD->getExternLoc().getLocWithOffset(keywordOffset);
      } else if (CTSD->isExplicitSpecialization()) {
        // Don't try to export what could just be a template meta programming class
        if (!requiresExport)
          return true;
      }
    } else {
      auto *nestedName = D->getQualifier();
      // Check if the class name is prefixed with a type or namespace 
      if (nestedName) {
        // Use namespace prefix starting location to insert at instead of the 
        // normal class name location that skips the name prefixes
        insertion_point = D->getQualifierLoc().getBeginLoc();
      }
    }

    if (insertion_point.isMacroID()) {
      if (debuglog) {
        llvm::outs() << "Skip exporting '" << D->getDeclName() << "' the macro insertion point was inside a macro.\n";
      }
      return true;
    }

    if (D->isClass() || D->isStruct()) {
      location = context_.getFullLoc(D->getLocation()).getExpansionLoc();

      if (D->needsImplicitCopyConstructor() || D->needsImplicitCopyAssignment() || D->needsImplicitMoveAssignment() || D->needsImplicitDestructor()) {
        sema->ForceDeclarationOfImplicitMembers(D);

        for (const auto* ctor : D->ctors()) {
          if (ctor->isImplicit() && ctor->isDeleted()) {
            // do something
          }
        }
      }

      unexported_public_interface(location)
        << D
        << clang::FixItHint::CreateInsertion(insertion_point,
          exportMacro + " ");
      return true;
    }

    return true;
  }

  bool ShouldSkipDeclaration(clang::Decl *D) {
    clang::FullSourceLoc location = get_location(D);

    if (isLocationIgnored(location))
      return true;

    if (location.isMacroID())
      return true;

    // Doon't export declarations contained in anonymous namespaces
    if (D->isInAnonymousNamespace())
      return true;

    if (D->isImplicit())
      return true;
    
    if (D->isTemplateDecl())
      return true;


    return false;
  }

  bool VisitVarDecl(clang::VarDecl *VD) {

    if (VD->isCXXClassMember())
      return true;

    clang::FullSourceLoc location = get_location(VD);

    const auto filename = location.getFileEntryRef()->getName();
    int line = location.getLineNumber();

    if (isLocationIgnored(location))
      return true;

    if (isAlreadyExported(VD, true))
      return true;
    auto context = VD->getDeclContext();

    if (clang::isa_and_nonnull<clang::RecordDecl>(VD))
      return true;

    // Doon't export declarations contained in anonymous namespaces
    if (VD->isInAnonymousNamespace())
      return true;
    
    if (!VD->hasExternalStorage()) {
      // Only annotate global variable definitions in source files not headers
      if (llvm::sys::path::extension(filename) != ".cpp") {
        return true;
      }
    }
    clang::SourceLocation insertion_point = VD->getBeginLoc();
    unexported_public_interface(location)
      << VD
      << clang::FixItHint::CreateInsertion(insertion_point,
        "LLVM_ABI ");
    return true;
  }

  void LogSkippedDecl(clang::NamedDecl *D, clang::FullSourceLoc location, const std::string &reason) {
    if (debuglog) {
      llvm::dbgs() << "Skipping " << D->getName() << reason << "that was declared in " << location.getFileEntry()->getName()
                   << " line " << location.getLineNumber() << "\n";
    }
  }

  bool VisitClassTemplateDecl(clang::ClassTemplateDecl *CTD) {
    //llvm::dbgs() << "ClassTemplateDecl: " << CTD->getNameAsString() << " is definition: " << CTD->isThisDeclarationADefinition() << " visible: " << CTD->isExternallyVisible() << "\n";
    return true;
  }

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    clang::FullSourceLoc location = get_location(FD);

    if (ShouldSkipDeclaration(FD))
      return true;

    if (isAlreadyExported(FD, FD->isCXXClassMember()))
      return true;

    // We are only interested in non-dependent types.
    if (FD->isDependentContext())
      return true;

    // If the function has a body, it can be materialized by the user.
    if (FD->hasBody())
      return true;

    if (FD->hasAttr<BuiltinAttr>()) {
      if (debuglog) {
        llvm::outs() << "Skipping builtin: " << FD->getName() << '\n';
      }
      return true;
    }

    if (llvm::isa<clang::FunctionTemplateDecl>(FD)) {
      
    }

    if (FD->isCXXClassMember()) {
      // Ignore deleted and defaulted functions (e.g. operators).
      if (FD->isDeleted() || FD->isDefaulted())
        return true;

      // Ignore friend declarations.
      if (const auto *FR = llvm::dyn_cast<clang::FriendDecl>(FD)) {
        const auto *target = FR->getFriendDecl();
        if (target && isAlreadyExported(target, true)) {
          return true;
        }
      } else {
        // FIXME add support for user requested exporting of individual members of a class
        return true;
      }

      if (const auto *MD = llvm::dyn_cast<clang::CXXMethodDecl>(FD)) {
        // Ignore private members (except for a negative check).
        if (MD->getAccess() == clang::AccessSpecifier::AS_private) {
          // TODO(compnerd) this should also handle `__visibility__`
          if (MD->hasAttr<clang::DLLExportAttr>())
            // TODO(compnerd) this should emit a fix-it to remove the attribute
            exported_private_interface(location) << MD;
          return true;
        }

        // Pure virtual methods cannot be exported.
        if (MD->isPureVirtual())
          return true;
      }
    } else if (FD->isExternC()) {
      // Don't export extern "C" declared functions by default
      if (!export_extern_c) {
        return true;
      }
    }

    // TODO(compnerd) replace with std::set::contains in C++20
    if (contains(get_ignored_functions(), FD->getNameAsString()))
      return true;

    clang::SourceLocation insertion_point = insertion_point = FD->getBeginLoc();
    if (FD->getTemplatedKind() != clang::FunctionDecl::TK_NonTemplate) {
      insertion_point = FD->getInnerLocStart();
    } else if(func_macro_on_name) {
      auto *nestedName = FD->getQualifier();
      // Check if the function name is prefixed with a type or namespace 
      if (nestedName) {
        // Use namespace prefix starting location to insert at instead of the 
        // normal class name location that skips the name prefixes
        insertion_point = FD->getQualifierLoc().getBeginLoc();
      } else {
        insertion_point = FD->getNameInfo().getBeginLoc();
      }
    }

    unexported_public_interface(location)
        << FD
        << clang::FixItHint::CreateInsertion(insertion_point,
                                             function_export_macro + " ");
    return true;
  }

  void SetSema(clang::Sema& sema) {
    this->sema = &sema;
  }

  bool isAlreadyExported(const clang::Decl *D, bool ignoreInherited) {
    for (auto *Atrr : D->attrs()) {
      if (clang::isa<DLLExportAttr>(Atrr) || clang::isa<DLLExportAttr>(Atrr) || 
          clang::isa<VisibilityAttr>(Atrr)) {
        if (Atrr->isInherited()) {
          if (debuglog) {
            llvm::StringRef name = D->getDeclKindName();
            if (auto *namedDecl = clang::dyn_cast<clang::NamedDecl>(D))
              name = namedDecl->getName();
            llvm::outs() << "Ignored inherited export attribute on: " << name << '\n';
          }
          continue;
        }

        auto range = D->getSourceRange();
        clang::FullSourceLoc location = context_.getFullLoc(Atrr->getLocation()).getExpansionLoc();
        // The source range we get for functions and variables starts after there attributes
        if (!clang::isa<clang::CXXRecordDecl>(D) || range.fullyContains(location)) {
          return true;
        } else {
          continue;
        }
      }

    }
    return false;
  }

  bool isLocationIgnored(clang::FullSourceLoc loc) {
    if (source_manager_.isInSystemHeader(loc))
      return true;

    if (mainfileonly && !isInsideMainFile(loc))
      return true;

    return false;
  }

  bool isInsideMainFile(clang::SourceLocation Loc) {
    if (!Loc.isValid())
      return false;
    clang::FileID FID = source_manager_.getFileID(source_manager_.getExpansionLoc(Loc));
    return FID == source_manager_.getMainFileID() || FID == source_manager_.getPreambleFileID();
  }
};

class RewritesReceiver : public clang::edit::EditsReceiver {
  Rewriter &Rewrite;

public:
  RewritesReceiver(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  void insert(SourceLocation loc, StringRef text) override {
    Rewrite.InsertText(loc, text);
  }

  void replace(CharSourceRange range, StringRef text) override {
    Rewrite.ReplaceText(range.getBegin(), Rewrite.getRangeSize(range), text);
  }
};

class consumer : public clang::SemaConsumer {
  struct fixit_options : clang::FixItOptions {
    fixit_options() {
      InPlace = inplace;
      Silent = apply_fixits;
    }

    std::string RewriteFilename(const std::string &filename, int &fd) override {
      llvm_unreachable("unexpected call to RewriteFilename");
    }
  };

  idt::visitor visitor_;

  fixit_options options_;
  std::unique_ptr<clang::FixItRewriter> rewriter_;

public:
  explicit consumer(clang::ASTContext &context, bool skipFunctionBodies, llvm::StringMap<std::string> &allFileChanges)
      : visitor_(context, skipFunctionBodies), filechanges(allFileChanges) {}


  llvm::StringMap<std::string>& filechanges;

  void HandleTranslationUnit(clang::ASTContext &context) override {
    SourceManager& SM = context.getSourceManager();

    if (apply_fixits) {
      clang::DiagnosticsEngine &diagnostics_engine = context.getDiagnostics();
      rewriter_ =
          std::make_unique<clang::FixItRewriter>(diagnostics_engine,
                                                 SM,
                                                 context.getLangOpts(),
                                                 &options_);
      diagnostics_engine.setClient(rewriter_.get(), /*ShouldOwnClient=*/false);
    }

    visitor_.TraverseDecl(context.getTranslationUnitDecl());

    if (apply_fixits) {
      RewritesReceiver Rec(rewriter_->Rewrite);
      rewriter_->Editor.applyRewrites(Rec);

      for (auto I = rewriter_->buffer_begin(), 
                E = rewriter_->buffer_end(); I != E; I++) {
        OptionalFileEntryRef Entry = SM.getFileEntryRefForID(I->first);

        llvm::SmallString<255> name = Entry->getName();
        llvm::sys::path::native(name);

        auto pair = filechanges.try_emplace(name);
        if (pair.second) {
          std::string output;
          llvm::raw_string_ostream OS(output);

          RewriteBuffer &RewriteBuf = I->second;
          RewriteBuf.write(OS);
          OS.flush();

          pair.first->second = std::move(output);
        } else {
          llvm::outs() << "Skipped duplicate write to: " << name << '\n';
        }
      }

    }
  }

  void InitializeSema(clang::Sema& sema) override {
    visitor_.SetSema(sema);
  }
};

struct action : clang::ASTFrontendAction {
  llvm::StringMap<std::string>& filechanges;

  action(llvm::StringMap<std::string> &filechanges) 
    : filechanges(filechanges) {

  }

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef InFile) override {
    llvm::outs() << "Processing: " << InFile << '\n';
    CI.getPreprocessor().SetSuppressIncludeNotFoundError(true);

    bool skipFunctionBodies = true;
    CI.getFrontendOpts().SkipFunctionBodies = skipFunctionBodies;
    DiagnosticsEngine &Diag = getCompilerInstance().getDiagnostics();
    Diag.setSeverity(clang::diag::warn_unused_private_field, diag::Severity::Ignored, SourceLocation());

    return std::make_unique<idt::consumer>(CI.getASTContext(), skipFunctionBodies, filechanges);
  }
};

struct factory : clang::tooling::FrontendActionFactory {
  llvm::StringMap<std::string> allfilechanges;
  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<idt::action>(allfilechanges);
  }
};
}

class IncludeFinder : public clang::PPCallbacks {

public:
  explicit IncludeFinder(clang::Preprocessor *PP) 
    : PP(PP), SM(PP->getSourceManager()) {
  }

  void InclusionDirective(clang::SourceLocation HashLoc,
                          const clang::Token &IncludeTok,
                          clang::StringRef FileName, bool IsAngled,
                          clang::CharSourceRange FilenameRange,
                          clang::OptionalFileEntryRef File,
                          clang::StringRef SearchPath,
                          clang::StringRef RelativePath,
                          const clang::Module *Imported,
                          clang::SrcMgr::CharacteristicKind FileType) override {

    if (!File || IsAngled || SM.isInSystemHeader(HashLoc)) {
      return; 
    }

    if (!File) return; // Handle cases where the included file is not found
    llvm::SmallString<256> nativePath = File->getName();
    llvm::sys::path::native(nativePath);
 
    IncludedHeaders.insert(nativePath);

    if (File->getName().contains("llvm") || File->getName().contains("clang")) {
      //llvm::outs() << "Included file: " << File->getName() << "\n";
    }

  }

  void FileChanged(SourceLocation Loc, FileChangeReason Reason,
                   SrcMgr::CharacteristicKind NewFileType, FileID prevId) override {

    if (Reason != EnterFile)
      return;
    auto fullloc = FullSourceLoc(Loc, SM);
    FileID Id = fullloc.getFileID();
    auto file = fullloc.getFileEntryRef();
    if (file && !fullloc.isInSystemHeader()) {
      auto name = file->getName();
     // llvm::outs() << "  Entered: " << name << "\n";
    } else {
      return;
    }
  }

  llvm::StringSet<> IncludedHeaders;
private:
  clang::SourceManager &SM;
  clang::Preprocessor *PP;
};

typedef std::pair<std::string, llvm::StringSet<>> FileIncludeResults;

class FindIncludesAction : public clang::PreprocessorFrontendAction {
public:
  FindIncludesAction(std::vector<FileIncludeResults>& includeList)
    : headerIncludes(includeList) {

  }
protected:
  void ExecuteAction() override {

    clang::CompilerInstance &CI = getCompilerInstance();
    auto &PP = CI.getPreprocessor();

    auto includeFinders = new IncludeFinder(&PP);
    PP.addPPCallbacks(std::unique_ptr<PPCallbacks>(includeFinders));
    PP.IgnorePragmas();

    // First let the preprocessor process the entire file and call callbacks.
    // Callbacks will record which #include's were actually performed.
    PP.EnterMainSourceFile();
    Token Tok;
    // Only preprocessor directives matter here, so disable macro expansion
    // everywhere else as an optimization.
    // TODO: It would be even faster if the preprocessor could be switched
    // to a mode where it would parse only preprocessor directives and comments,
    // nothing else matters for parsing or processing.
    PP.SetMacroExpansionOnlyInDirectives();
    do {
      PP.Lex(Tok);
      //if (Tok.is(tok::annot_module_begin))
       // Rewrite->handleModuleBegin(Tok);
    } while (Tok.isNot(tok::eof));
   // RewriteMacrosInInput(CI.getPreprocessor(), OS.get());

    auto &SM = PP.getSourceManager();
    const auto &file = SM.getFileEntryRefForID(SM.getMainFileID());
    headerIncludes.push_back(std::make_pair(file->getName().str(), includeFinders->IncludedHeaders));
  }

public:
  std::vector<FileIncludeResults> &headerIncludes;
};


bool GatherHeadersInDirectory(llvm::StringRef directory, std::vector<std::string> &headerPaths) {
  using namespace llvm::sys::fs;
  using namespace llvm::sys;
  std::error_code ec;

  for (recursive_directory_iterator F(directory, ec), E; F != E; F.increment(ec)) {
    if (ec) {
      llvm::errs() << "Directory iterator error'ed when trying to find headers: " << ec.message();
      return false;
    }
    std::string path = F->path();
    if (F->type() == file_type::regular_file) {
      if (path::extension(path) == ".h") {
        headerPaths.push_back(F->path());
      } else {
        llvm::outs() << "Skipped non header file: " << F->path() << "\n";
      }
    }
  }
  return true;
}

class FindIncludesFrontendActionFactory : public clang::tooling::FrontendActionFactory {
public:
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<FindIncludesAction>(headerIncludes);
  }

  std::vector<std::pair<std::string, llvm::StringSet<>>> headerIncludes;
};

bool GatherHeaders(clang::tooling::CommonOptionsParser &options, std::vector<std::string>& rootheaders) {
  using namespace llvm::sys::fs;
  using namespace clang::tooling;

  std::vector<std::string> files;
  if (!is_directory(header_directory)) {
    llvm::errs() << "Header directory \"" << header_directory << "\" does not exist\n";
    return false;
  }

  llvm::SmallString<256> NativePath;
  llvm::sys::path::native(header_directory, NativePath);
  if (!GatherHeadersInDirectory(NativePath, files)) {
    return false;
  }
  auto factory = std::make_unique<FindIncludesFrontendActionFactory>();

  auto excludes = get_ignored_headers();
  auto end = llvm::remove_if(files, [&](std::string &s) {
    StringRef RelativePath = llvm::StringRef(s).substr(NativePath.size() + 1);
    for (const auto& Pat : excludes) {
      if (Pat.match(RelativePath)) {
        llvm::errs() << "Skipped ignored header: " << s << "\n";
        return true;
      }
    }
    
    return false;
  });
  files = std::vector(files.begin(), end);

  ClangTool tool2(options.getCompilations(), files);
  tool2.run(factory.get());

  llvm::StringSet<> nonroots;
  for (auto header : factory->headerIncludes) {
    for (auto &path : header.second) {
      nonroots.insert(path.getKey());
    }
  }

  for (auto &path : files) {
    if (!nonroots.contains(path)) {
      rootheaders.push_back(path);
    }
  }

  return true;
}

class MemCDB : public clang::tooling::CompilationDatabase {
public:
  MemCDB(clang::tooling::CompilationDatabase &db) 
    : DB(db) {
  }

  std::vector<clang::tooling::CompileCommand> getCompileCommands(StringRef F) const override {
    return DB.getCompileCommands(F);
  }

  std::vector<std::string> getAllFiles() const override {
    return DB.getAllFiles();
  }

  std::vector<clang::tooling::CompileCommand> getAllCompileCommands() const override {
    throw std::logic_error("The method or operation is not implemented.");
  }

private:
  clang::tooling::CompilationDatabase& DB;
};

int main(int argc, char *argv[]) {
  using namespace clang::tooling;


  auto options =
      CommonOptionsParser::create(argc, const_cast<const char **>(argv),
                                  idt::category, llvm::cl::OneOrMore);

  if (!options) {
    llvm::logAllUnhandledErrors(std::move(options.takeError()), llvm::errs());
    return EXIT_FAILURE;
  }

  if (function_export_macro == "") {
    function_export_macro = export_macro.getValue();
  }

  auto InferedDB = inferMissingCompileCommands(std::make_unique<MemCDB>(options->getCompilations()));

  std::unique_ptr<ClangTool> tool;
  if (!header_directory.empty()) {
    std::vector<std::string> rootheaders;
    if (!GatherHeaders(options.get(), rootheaders)) {
      return EXIT_FAILURE;
    }

    std::string buffer;
    buffer.reserve(1024);
    llvm::raw_string_ostream stream(buffer);

    for (auto &path : rootheaders) {
      llvm::SmallString<256> slashpath;
      llvm::sys::path::native(path, slashpath, llvm::sys::path::Style::windows_slash);

      stream << "#include \"" << slashpath << "\"\n";
      llvm::outs() << "RootHeader: " << slashpath << "\n";
    }
    stream.flush();

    if (true) {


      tool = std::make_unique<ClangTool>(ClangTool(*InferedDB.get(), { rootheaders }));

      
    } else {
      ClangTool tool{ options->getCompilations(), { "DummyCombined.cpp"} };
      tool.mapVirtualFile("DummyCombined.cpp", buffer);
    }
  } else {
    tool = std::make_unique<ClangTool>(ClangTool(*InferedDB.get(), { options->getSourcePathList() }));
    if (mainfileonly.getNumOccurrences() == 0) {
      mainfileonly = true;
    }
  }

  int result;
  auto factory = std::make_unique<idt::factory>();
  result = tool->run(factory.get());

  if (!factory->allfilechanges.empty()) {
    llvm::outs() << "Files to modify:\n";
    for (auto & pair : factory->allfilechanges) {
      llvm::StringRef filename = pair.first();
      llvm::outs() << "  " << filename << '\n';
    }
    char c;
    llvm::outs() << "Commit changes Yes, No?\n";
    if (!std::cin.get(c) || c != 'y') {
      return 1;
    }
  }

  for (auto & pair : factory->allfilechanges){
    std::error_code EC;
    std::unique_ptr<llvm::raw_fd_ostream> OS;
    llvm::StringRef filename = pair.first();
    OS.reset(new llvm::raw_fd_ostream(filename, EC, llvm::sys::fs::OF_None));
    if (EC) {
      llvm::errs() << "Unable to open" << filename << EC.message();
      continue;
    }
    OS->write(pair.second.c_str(), pair.second.size());
    OS->flush();
  }

  return result;

 // Clang->getPreprocessorOpts().addRemappedFile("<<< inputs >>>", MB);
  //auto MB = llvm::MemoryBuffer::getMemBuffer(buffer);
}
