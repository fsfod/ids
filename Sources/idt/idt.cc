// Copyright (c) 2021 Saleem Abdulrasool.  All Rights Reserved.
// SPDX-License-Identifier: BSD-3-Clause

#include "ExportOptionsConfig.h"
#include "FindIncludes.h"
#include "FixItRewriter2.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Edit/EditsReceiver.h"
#include "clang/Format/Format.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring/AtomicChange.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Transformer/RewriteRule.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/GlobPattern.h"
#include "llvm/Support/Path.h"

#include <cstdlib>
#include <iostream>
#include <set>
#include <string>

class ResultCollector : public ThreadSafeToolResults<std::string, std::string> {
public:
  ResultCollector(FileOptionLookup *fileOptions, BaseExportOptions *exportOptions = nullptr)
    : FileExportOptions(fileOptions), DefaultExportOptions(exportOptions) {
  }

  ResultCollector(BaseExportOptions *exportOptions)
    : FileExportOptions(nullptr), DefaultExportOptions(exportOptions) {
  }

  BaseExportOptions *getFileExportOptions(clang::FileEntryRef file) {
    if (!FileExportOptions) {
      return DefaultExportOptions;
    }

    auto it = FileExportOptions->find(file.getUniqueID());
    return it != FileExportOptions->end() ? it->second : NULL;
  }

  BaseExportOptions *getDefaultExportOptions() {
    return DefaultExportOptions;
  }

protected:
  FileOptionLookup *FileExportOptions;
  BaseExportOptions *DefaultExportOptions;
};

namespace idt {
llvm::cl::OptionCategory category{"interface definition scanner options"};
}

BaseExportOptions baseOptions;

namespace {

llvm::cl::opt<std::string, true>
export_macro("export-macro",
             llvm::cl::desc("The macro to decorate interfaces with"),
             llvm::cl::value_desc("define"), llvm::cl::Optional,
             llvm::cl::cat(idt::category), llvm::cl::location(baseOptions.ExportMacro));

llvm::cl::opt<std::string, true>
class_export_macro("class-export-macro",
  llvm::cl::desc("The macro to decorate interfaces with"),
  llvm::cl::value_desc("define"), llvm::cl::Optional,
  llvm::cl::cat(idt::category), llvm::cl::location(baseOptions.ClassMacro));

llvm::cl::opt<std::string, true>
template_export_macro("extern-template-macro",
  llvm::cl::desc("The macro to decorate extern class/function templates with"),
  llvm::cl::value_desc("define"), llvm::cl::Optional,
  llvm::cl::cat(idt::category), llvm::cl::location(baseOptions.ExternTemplateMacro));

llvm::cl::opt<std::string, true>
externc_export_macro("externc_export_macro",
  llvm::cl::desc("The macro to decorate functions declared in extern \"C\" context "),
  llvm::cl::value_desc("define"), llvm::cl::Optional,
  llvm::cl::cat(idt::category), llvm::cl::location(baseOptions.ExternCMacro));

llvm::cl::opt<bool>
apply_fixits("apply-fixits", llvm::cl::init(false),
             llvm::cl::desc("Apply suggested changes to decorate interfaces"),
             llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
inplace("inplace", llvm::cl::init(false),
        llvm::cl::desc("Apply suggested changes in-place"),
        llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
interactive("interactive", llvm::cl::init(false),
  llvm::cl::desc("Display list of changed files at the end and ask for confirmation to write them"),
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
root_header_directory("header-dir",
  llvm::cl::desc("Directory to recursively search for headers to rewrite"),
  llvm::cl::value_desc("define"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<std::string>
export_config_path("export-config",
  llvm::cl::desc("Path of export config file"),
  llvm::cl::value_desc("define"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<std::string>
export_group_to_run("export-group",
  llvm::cl::desc("Name of export group to generate for"),
  llvm::cl::cat(idt::category));

llvm::cl::list<std::string>
ignored_headers("header-ignore",
  llvm::cl::desc("Ignore one or more header files"),
  llvm::cl::value_desc("header[,header...]"),
  llvm::cl::CommaSeparated,
  llvm::cl::cat(idt::category));

llvm::cl::list<std::string>
ignored_classes("ignore-class",
  llvm::cl::desc("Ignore one or more class names"),
  llvm::cl::value_desc("class[,class...]"),
  llvm::cl::CommaSeparated,
  llvm::cl::cat(idt::category));

llvm::cl::list<std::string>
compdb_pathmatch("compdb-pathmatch",
  llvm::cl::desc("File path glob patten of files to process from compilation database"),
  llvm::cl::value_desc("patten[,patten...]"),
  llvm::cl::CommaSeparated,
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
export_extern_c("export-extern-c", llvm::cl::init(false),
  llvm::cl::desc("Add export macros to extern C declarations"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
skip_simple_classes("skip-simple-classes", llvm::cl::init(true),
  llvm::cl::desc("Skip exporting simple classes and structs that "
    "don't have any out of line methods or static fields"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<bool>
func_macro_on_name("function-macro-on-name", llvm::cl::init(false),
  llvm::cl::desc("Change the attach point for function export macros to next to there name"),
  llvm::cl::cat(idt::category));

llvm::cl::opt<int> thread_parallelism("threads", llvm::cl::init(0),
  llvm::cl::desc("Number of threads to use to process headers and source files"),
  llvm::cl::cat(idt::category));

llvm::cl::alias threadsJ("j", llvm::cl::desc("Alias for -threads"), llvm::cl::aliasopt(thread_parallelism));

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
static llvm::StringMap<llvm::SmallVector<std::string, 1>> IgnoredRecordNames;

static bool BuildIgnoredCXXRecordNames() {
  for (auto& name : ignored_classes) {
    llvm::StringRef nameRef = name;
    size_t namespaceEnd = nameRef.rfind("::");
    if (namespaceEnd != llvm::StringRef::npos) {
      if (namespaceEnd > (nameRef.size() - 2)) {
        llvm::errs() << "Bad class name '" << name << "' missing name after namespace prefix";
        return false;
      }
      IgnoredRecordNames[nameRef.substr(namespaceEnd + 2)].push_back(name);
    }
    else {
      IgnoredRecordNames[name].push_back("");
    }
  }
  return true;
}


namespace idt {
class visitor : public clang::RecursiveASTVisitor<visitor> {
  clang::ASTContext &context_;
  clang::SourceManager &source_manager_;
  clang::Sema *sema;
  BaseExportOptions &options;
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
  explicit visitor(clang::ASTContext &context, BaseExportOptions &options,  bool skipFuncBodies)
      : context_(context), source_manager_(context.getSourceManager()), sema(nullptr), 
        skip_function_bodies(skipFuncBodies), options(options) {

    auto path = source_manager_.getFileEntryRefForID(source_manager_.getMainFileID())->getName();
    llvm::StringRef extension = llvm::sys::path::extension(path);
    if (extension == ".cpp" || extension == ".cxx") {
      isMainFileAHeader = false;
    }
  }

  bool debuglog = false;
  bool isMainFileAHeader = true;

  bool IsCXXRecordNameIngored(clang::CXXRecordDecl *D) {
    auto it = IgnoredRecordNames.find(D->getName());
    if (it != IgnoredRecordNames.end()) {
      auto nameVec = it->getValue();

      for (auto& name : nameVec) {
        if (name == "")
          return true;

      }
    }
    return false;
  }

  bool VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (D->isEnum() || !isMainFileAHeader)
      return true;

    // We won't visit some forward declared classes again that get a definition in same translation unit
    if (!D->isThisDeclarationADefinition()) {
      return true;
    }

    clang::FullSourceLoc location = context_.getFullLoc(D->getLocation()).getExpansionLoc();

    if (ShouldSkipDeclaration(D))
      return true;

    auto parent = D->getDeclContext();
    // Skip non templated class/structs in a templated type
    if (D->isDependentContext())
      return true;

    auto *CTSD = clang::dyn_cast<clang::ClassTemplateSpecializationDecl>(D);
    // Process extern template declarations separately when we visit them directly
    if (CTSD && (CTSD->getTemplateSpecializationKind() == clang::TSK_ExplicitInstantiationDeclaration || 
                CTSD->getTemplateSpecializationKind() == clang::TSK_ExplicitInstantiationDefinition)) {
      return true;
    }

    bool alreadyExported = isAlreadyExported(D, true);

    // Do we still need to check for exporting individual members
    if (alreadyExported && !options.ForceExportClassData)
      return true;

    llvm::SmallVector<clang::Decl*> unexported;
    ExportedStats exportCounts;
    auto status = GetUnexportedMembers(D, unexported, exportCounts);

    // If some methods or static variables are already individually exported continue to export any new 
    // methods or variables as well instead of exporting the class.
    bool exportMethods = !alreadyExported && (status == UnexportedStatus::Partial || options.ExportMembers);

    // Force exported static variables even if the class is alreadyed exported
    if ((options.ForceExportClassData && exportCounts.UnexportedVariables) || exportMethods) {
      for (clang::Decl* member : unexported) {
        auto *F = clang::dyn_cast<clang::FunctionDecl>(member);
        if (F && exportMethods) {
          ExportFunction(F);
        } else if (auto *V = clang::dyn_cast<clang::VarDecl>(member)) {
          if (V->isConstexpr())
            continue;
          ExportVariable(V, options.ClassDataMacro);
        }
      }
    }

    // Don't try to export the class directly if individual members are exported
    if (exportCounts.ExportedMethods || exportCounts.ExportedVariables || status == UnexportedStatus::Partial)
      return true;

    if (alreadyExported || options.ExportMembers)
      return true;

    bool requiresExport = status != UnexportedStatus::None || D->isAbstract();

    // Don't add DLL export to PoD structs that also have no methods
    if (skip_simple_classes && !requiresExport) {
      LogSkippedDecl(D, location, " that only has inline members");
      return true;
    }

    // Check if this ia nested class or struct
    if (const clang::RecordDecl *PD = clang::dyn_cast<clang::RecordDecl>(parent)) {
      assert(PD->isClass() || PD->isStruct() || PD->isUnion());

      // Skip nested class and structs that have no methods or there all declared inside the declaration
      if (!requiresExport) {
        return true;
      }
    }

    if (CTSD) {
      if (debuglog) {
        llvm::outs() << "TemplateDecl: " << CTSD->getNameAsString() << ", Body: " << CTSD->hasBody()
          << ", Definition: " << CTSD->hasDefinition() << ", Specialization Kind: "
          << CTSD->getSpecializationKind() << "\n";
      }

      // We only want export full class specialization
      if (llvm::isa<clang::ClassTemplatePartialSpecializationDecl>(CTSD))
        return true;

      // Skip nested classes
      if (CTSD->isClassScopeExplicitSpecialization())
        return true;

      if (CTSD->isExplicitSpecialization()) {
        // Don't try to export what could just be a template meta programming class
        if (!requiresExport)
          return true;
      }
    }

    if (D->isClass() || D->isStruct()) {
      D->hasNonTrivialCopyAssignment();

      if (D->needsImplicitCopyConstructor() || D->needsImplicitCopyAssignment() || D->needsImplicitMoveAssignment() || D->needsImplicitDestructor()) {
        sema->ForceDeclarationOfImplicitMembers(D);

        for (const auto* ctor : D->ctors()) {
          if (ctor->isImplicit() && ctor->isDeleted()) {
            // do something
          }
        }
      }
    }

    ExportClass(D);
    return true;
  }

  bool VisitClassTemplateDecl(clang::ClassTemplateDecl *D) {
    if (!options.ExportTemplates)
      return true;

    if (ShouldSkipDeclaration(D, true))
      return true;

    clang::CXXRecordDecl *CRD = D->getTemplatedDecl();
    if (isAlreadyExported(CRD, true))
      return true;

    clang::VarDecl *unexportedStatic = FirstUnexportedStaticField(CRD);

    if (!unexportedStatic)
      return true;

    if (debuglog) {
      llvm::outs() << "Found unexported template class " << D->getName() << " with static field " << unexportedStatic->getName() << "\n";
    }

    ExportClass(D);
    return true;
  }

  void ExportClass(clang::NamedDecl *D, llvm::StringRef exportMacro = "") {
    clang::FullSourceLoc location = GetFullExpansionLoc(D->getLocation());

    if (exportMacro.empty()) {
      exportMacro = options.ClassMacro.empty() ? options.ExportMacro : options.ClassMacro;
    }

    clang::SourceLocation insertion_point = D->getLocation();

    if (auto *TD = clang::dyn_cast<clang::TagDecl>(D)) {
      auto *nestedName = TD->getQualifier();
      // Check if the class name is prefixed with a type or namespace 
      if (nestedName) {
        // Use namespace prefix starting location to insert at instead of the 
        // normal class name location that skips the name prefixes
        insertion_point = TD->getQualifierLoc().getBeginLoc();
      }
    }

    // If were parsing with clang __declspec can't go before some attributes
    if (D->hasAttrs()) {
      auto maxStart = source_manager_.getLocForEndOfFile(source_manager_.getFileID(location));
      clang::SourceLocation attrStart = maxStart;
      clang::Attr *firstAttr;

      for (auto *Atrr : D->attrs()) {
        if (Atrr->isInherited() || Atrr->isDeclspecAttribute())
          continue;

        auto *align = clang::dyn_cast<clang::AlignedAttr>(Atrr);
        if(!Atrr->isGNUAttribute() && !Atrr->isCXX11Attribute() && !(align && align->isKeywordAttribute()))
          continue;

        clang::SourceRange range = Atrr->getRange();

        auto start = GetFullExpansionLoc(range.getBegin());
        if (start < attrStart) {
          attrStart = start;
          firstAttr = Atrr;
        }
      }

      if (attrStart != maxStart) {
        // If the attribute is expanded from macro location won't be offset to attribute name
        if (firstAttr->getLoc().isMacroID() || firstAttr->isKeywordAttribute()) {
          insertion_point = attrStart;
        } else if (firstAttr->isGNUAttribute()) {
          // __attribute__((
          insertion_point = attrStart.getLocWithOffset(-15);
        } else if(firstAttr->isCXX11Attribute()) {
          // [[
          insertion_point = attrStart.getLocWithOffset(-2);
        } else {
          LogSkippedDecl(D, location, "' unhandled attribute type to adjust insertion point for");
          return;
        }
      }
    }

    if (insertion_point.isMacroID()) {
      if (debuglog) {
        LogSkippedDecl(D, location, "' macro insertion point was inside another macro");
      }
      return;
    }

    unexported_public_interface(location)
      << D
      << clang::FixItHint::CreateInsertion(insertion_point,
        (exportMacro + " ").str());
  }

  void ExportFunction(clang::FunctionDecl *D, llvm::StringRef exportMacro = "") {
    clang::FullSourceLoc location = GetFullExpansionLoc(D->getLocation());

    if (exportMacro.empty()) {
      exportMacro = options.ExportMacro;
    }

    bool hasAttributes = false;
    for (auto *Atrr : D->attrs()) {
      if (Atrr->isInherited() || Atrr->isDeclspecAttribute())
        continue;

      if (Atrr->isGNUAttribute() || Atrr->isCXX11Attribute()) {
        auto attribLoc = GetFullExpansionLoc(Atrr->getLoc());

        // If the attribute is after the parameters next to the opening brace
        // like this: char *getTraitName(TypeTrait T) __attribute__((__pure__));
        // we have to keep the export macro before the return type
        if (attribLoc > location) {
          continue;
        }
        hasAttributes = true;
        break;
      }      
    }

    clang::SourceLocation insertion_point = insertion_point = D->getBeginLoc();
    if (D->getTemplatedKind() != clang::FunctionDecl::TK_NonTemplate) {
      insertion_point = D->getInnerLocStart();
    } else if (func_macro_on_name || hasAttributes) {
      auto *nestedName = D->getQualifier();
      // Check if the function name is prefixed with a type or namespace 
      if (nestedName) {
        // Use namespace prefix starting location to insert at instead of the 
        // normal class name location that skips the name prefixes
        insertion_point = D->getQualifierLoc().getBeginLoc();
      } else {
        insertion_point = D->getNameInfo().getBeginLoc();
      }
    }

    unexported_public_interface(location)
      << D
      << clang::FixItHint::CreateInsertion(insertion_point,
        (exportMacro + " ").str());
  }

  void ExportVariable(clang::VarDecl *D, llvm::StringRef exportMacro = "") {
    clang::FullSourceLoc location = GetFullExpansionLoc(D->getLocation());

    if (exportMacro.empty()) {
      if (!options.DataMacro.empty()) {
        exportMacro = options.DataMacro;
      } else {
        exportMacro = options.ExportMacro;
      }
    }

    clang::SourceLocation insertion_point = D->getBeginLoc();
    unexported_public_interface(location)
      << D
      << clang::FixItHint::CreateInsertion(insertion_point,
        (exportMacro + " ").str());
  }

  bool VisitClassTemplateSpecializationDecl(clang::ClassTemplateSpecializationDecl *D) {
    clang::FullSourceLoc location = get_location(D);
    const auto filename = location.getFileEntryRef()->getName();
    int line = location.getLineNumber();

    auto Kind = D->getTemplateSpecializationKind();
    std::string exportMacro;

    bool isDefinition = Kind == clang::TSK_ExplicitInstantiationDefinition;

    if ((isDefinition && isMainFileAHeader) ||
        (!isDefinition && Kind != clang::TSK_ExplicitInstantiationDeclaration))
      return true;

    if (ShouldSkipDeclaration(D))
      return true;

    clang::SourceRange defRange;

    if (isDefinition) {
      exportMacro = options.ExportTemplateMacro;
      defRange = clang::SourceRange(D->getTemplateKeywordLoc(), D->getLocation());
    } else {
      exportMacro = options.ExternTemplateMacro;
      if (!D->getExternKeywordLoc().isValid()) {
        LogSkippedDecl(D, location, "extern location is not valid");
        return true;
      }

      if (!D->hasExternalFormalLinkage()) {
        llvm::dbgs() << "NO external formal linkage\n";
        return true;
      }

      defRange = clang::SourceRange(D->getExternKeywordLoc(), D->getLocation());
    }

    // TODO is there a better way todo this
    auto sourceText = GetSourceTextForRange(defRange);
    // Don't apply the macro if it already has one
    if (sourceText.contains(exportMacro)) {
      return true;
    }

    clang::SourceLocation insertion_point ;

    if (isDefinition) {
      if (!TryGetNestedNameStartLoc(D, insertion_point)) {
        insertion_point = GetFullExpansionLoc(D->clang::NamedDecl::getLocation());
      } else {
        insertion_point = GetFullExpansionLoc(insertion_point);
      }
    } else {
      size_t keywordOffset = sourceText.find(D->isStruct() ? "struct" : "class");

      if (keywordOffset == llvm::StringRef::npos) {
        llvm::errs() << "Failed to find class or struct keyword for extern template declaration " << sourceText;
        return true;
      }

      // Insert one space after the keyword
      keywordOffset += D->isStruct() ? 7 : 6;
      insertion_point = D->getExternKeywordLoc().getLocWithOffset(keywordOffset);
    }

    unexported_public_interface(location)
      << D
      << clang::FixItHint::CreateInsertion(insertion_point,
        exportMacro + " ");

    return true;
  }

  bool ShouldSkipDeclaration(clang::Decl *D, bool allowTemplateDec = false, clang::SourceLocation location = clang::SourceLocation()) {
    clang::FullSourceLoc fullLocation;
    if (location.isInvalid()) {
      fullLocation = get_location(D);
    } else {
      fullLocation = context_.getFullLoc(location);
    }

    if (isLocationIgnored(fullLocation, D->getKind()))
      return true;

    if (location.isMacroID())
      return true;

    // Declarations in anonymous namespaces have no external linkage
    if (D->isInAnonymousNamespace())
      return true;

    if (D->isImplicit())
      return true;

    if (!allowTemplateDec && D->isTemplateDecl())
      return true;


    return false;
  }

  bool VisitVarDecl(clang::VarDecl *VD) {

    if (VD->isCXXClassMember() || clang::isa<clang::ParmVarDecl>(VD))
      return true;

    clang::FullSourceLoc location = get_location(VD);

    const auto filename = location.getFileEntryRef()->getName();
    int line = location.getLineNumber();

    if (isLocationIgnored(location, clang::Decl::Kind::Var))
      return true;

    if (isAlreadyExported(VD, true))
      return true;
    auto context = VD->getDeclContext();

    if (clang::isa_and_nonnull<clang::RecordDecl>(VD))
      return true;

    if (VD->isConstexpr())
      return true;

    // Doon't export declarations contained in anonymous namespaces
    if (VD->isInAnonymousNamespace())
      return true;

    clang::QualType type = VD->getType();

    if (!VD->hasExternalStorage()) {
      // By default exporting global variable definitions is opt in
      if (!options.ExportGlobalVariableDefs)
        return true;

      if (!VD->hasGlobalStorage() || type.isConstQualified() || VD->getStorageClass() == clang::SC_Static)
        return true;
      // Only annotate global variable definitions in source files not headers
      if (isMainFileAHeader) {
        return true;
      }
    }

    auto name = type.getAsString();

    if (!isMainFileAHeader && name.find("opt") == std::string::npos) {
      bool hasExtern = false;
      // Check if previous declarations of the variable that would be in a header has a export macro already applied
      // which implicitly requires us to also add a export macro to definition of the variable
      for (auto *V : VD->redecls()) {
        if (V->hasExternalStorage() && isAlreadyExported(V, true)) {
          hasExtern = true;
          break;
        }
      }
      if(!hasExtern)
        return true;
    }

    ExportVariable(VD);
    return true;
  }

  void LogSkippedDecl(clang::NamedDecl *D, clang::FullSourceLoc location, const std::string &reason) {
    if (debuglog) {
      llvm::dbgs() << "Skipping " << D->getName() << reason << ", declared in " << location.getFileEntryRef()->getName()
        << " line " << location.getLineNumber() << "\n";
    }
  }

  // Override TraverseFriendDecl so we can skip visiting the FunctionDecl stored by friend declarations,
  // Since our function visitor is not able to tell if a function declaration is a friend.
  bool TraverseFriendDecl(clang::FriendDecl *D) {
    if (!D->getFriendType() && clang::isa<clang::FunctionDecl>(D->getFriendDecl())) {
      VisitFriendDecl(D);
      return true;
    } else {
      RecursiveASTVisitor::TraverseFriendDecl(D);
    }
    return true;
  }

  bool VisitFriendDecl(clang::FriendDecl *D) {
    if (!isMainFileAHeader)
      return true;

    clang::FullSourceLoc location = get_location(D);
    
    if (isLocationIgnored(location, clang::Decl::Kind::Friend))
      return true;

    const auto *FD =
        clang::dyn_cast_if_present<clang::FunctionDecl>(D->getFriendDecl());

    if (!FD)
      return true;

    if (isAlreadyExported(FD, true))
      return true;

    auto *def = FD->getDefinition();

    if (def) {
      if (def->hasBody())
        return true;
      if (def->isInlined())
        return true;
    }

    auto tkind = FD->getTemplatedKind();
    // TODO: Is there any non dependent function templates that would be valid to 
    // add dllexport to that we would that see here.
    if (tkind != clang::FunctionDecl::TK_NonTemplate) {
      return true;
    }

    // Exclude class method that will rarely exported
    if (clang::isa<clang::CXXMethodDecl>(FD))
      return true;

    // If the function has a body, it can be materialized by the user.
    if (FD->isInlined())
      return true;

    if (FD->isInAnonymousNamespace())
      return true;
    
    clang::SourceLocation insertion_point = D->getFriendLoc().getLocWithOffset(7);
    unexported_public_interface(location)
      << FD
      << clang::FixItHint::CreateInsertion(insertion_point,
        options.ExportMacro + " ");
    return true;
  }


  bool LexExternTemplate(clang::Lexer &lexer, clang::SourceLocation &insertPoint) {
    clang::Token tok;

    bool found = false;
    while (!lexer.LexFromRawLexer(tok) || tok.is(clang::tok::eof)) {
      if (tok.is(clang::tok::raw_identifier) && tok.getRawIdentifier() == "extern") {
        found = true;
        break;
      }
    }

    if (!found)
      return false;

    if (lexer.LexFromRawLexer(tok) || tok.isNot(clang::tok::raw_identifier) || tok.getRawIdentifier() != "template")
      return false;

    insertPoint = tok.getEndLoc().getLocWithOffset(1);
    return true;
  }

  bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl *D) {
    if (D->isCXXClassMember())
      return true;

    clang::FullSourceLoc location = get_location(D);

    if (source_manager_.isInSystemHeader(location))
      return true;

    if (debuglog) {
      const auto filename = location.getFileEntryRef()->getName();
      int line = location.getLineNumber();
      llvm::outs() << "FuncTempl: " << D->getName() << " at " << filename << ":" << line << '\n';
    }

    for (auto *FD : D->specializations()) {
      for (auto *RD : FD->redecls()) {
        auto specKind = RD->getTemplateSpecializationKind();
        if (specKind == clang::TSK_ExplicitInstantiationDeclaration) {
          VisitExternFunctionTemplate(RD);
        } else if (specKind == clang::TSK_ExplicitInstantiationDefinition){
          VisitFunctionTemplateDefInstantiation(RD);
        }
      }
    }

    return true;
  }

  void VisitFunctionTemplateDefInstantiation(clang::FunctionDecl *D) {

    auto instLocation = GetFileLocation(D->getPointOfInstantiation());
    auto location = GetFileLocation(D->getLocation());

    if(isMainFileAHeader || ShouldSkipDeclaration(D, true, D->getPointOfInstantiation()))
      return;

    if (isAlreadyExported(D, true))
      return;
    D->dumpColor();

    clang::SourceLocation insert_point = D->getLocation();

    unexported_public_interface(D->getPointOfInstantiation())
      << D
      << clang::FixItHint::CreateInsertion(insert_point, options.ExportTemplateMacro + " ");
  }

  void VisitExternFunctionTemplate(clang::FunctionDecl *D) {
    auto instLocation = GetFileLocation(D->getPointOfInstantiation());
    int nameOffset = 0;
    clang::SourceRange lineRange = GetLineSourceRangeLoc(D->getPointOfInstantiation(), nameOffset);
    std::string lineText = GetSourceTextForRange(lineRange).str();
    llvm::StringRef declartionStart = lineText.substr(0, nameOffset - 1);

    if (isAlreadyExported(D, true))
      return;

    if(ShouldSkipDeclaration(D, true, D->getPointOfInstantiation()))

    if (debuglog) {
      llvm::outs() << "  Inst: " << instLocation.path << ":" << instLocation.line << '\n';
      llvm::outs() << "    \"" << lineText << '\"\n';
    }

    clang::Lexer lexer(lineRange.getBegin(), context_.getLangOpts(), lineText.c_str(), lineText.c_str(), lineText.c_str() + lineText.size());

    clang::SourceLocation insert_point;
    if (!LexExternTemplate(lexer, insert_point)) {
      llvm::outs() << "Failed to find start of guessed extern function template declaration for" << D->getName() << "in "
        << instLocation.path << ":" << instLocation.line << '\n';
      return;
    }

    if (lineText.find(options.ExternTemplateMacro) != std::string::npos) {
      return;
    }

    unexported_public_interface(D->getPointOfInstantiation())
      << D
      << clang::FixItHint::CreateInsertion(insert_point, options.ExternTemplateMacro + " ");
  }

  bool VisitCXXMethodDecl(clang::CXXMethodDecl * D) {
    auto spec = D->getTemplateSpecializationKind();
    // Were looking for explicit specialization member functions
    if (spec != clang::TSK_ExplicitSpecialization)
      return true;

    auto fileLoc = GetFileLocation(D->getInnerLocStart());

    if (D->hasBody())
      return true;

    if (D->isInlined())
      return true;

    if (ShouldSkipDeclaration(D))
      return true;
    
    clang::MemberSpecializationInfo *Info = D->getMemberSpecializationInfo();
    if (!Info)
      return true;
    
    auto *member = Info->getInstantiatedFrom();
    if (!member)
      return true;

    if (isAlreadyExported(D, false))
      return true;

    clang::FullSourceLoc location = get_location(D);

    unexported_public_interface(location)
      << D
      << clang::FixItHint::CreateInsertion(D->getInnerLocStart(),
        options.ExportMacro + " ");

    return true;
  }

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    clang::FullSourceLoc location = get_location(FD);

    if (ShouldSkipDeclaration(FD))
      return true;

    const clang::FunctionDecl* defOrDecl = FD->getDefinition();

    if (!defOrDecl)
      defOrDecl = FD;

    // If the function has a body, it can be materialized by the user.
    if (defOrDecl->hasBody() || defOrDecl->hasSkippedBody())
      return true;

    // hasBody can be false in lots of cases while the method is still declared inline of the class.
    if (defOrDecl->isInlined())
      return true;

    auto TSK = FD->getTemplateSpecializationKind();
    if (TSK == clang::TSK_ExplicitInstantiationDeclaration)
      return true;

    // Allow explicitly specialized class member functions declared out of line
    if (FD->isCXXClassMember() && TSK != clang::TSK_ExplicitSpecialization)
      return true;

    if (isAlreadyExported(FD, true))
      return true;

    // We are only interested in non-dependent types.
    if (FD->isDependentContext())
      return true;

    if (FD->hasAttr<clang::BuiltinAttr>()) {
      if (debuglog) {
        llvm::outs() << "Skipping builtin: " << FD->getName() << '\n';
      }
      return true;
    }

    std::string &exportMacro = options.ExportMacro;
    if (FD->isExternC()) {
      // Don't export extern "C" declared functions by default
      if (options.ExportExternC != true) {
        return true;
      }
      exportMacro = options.ExternCMacro;
    }

    // TODO(compnerd) replace with std::set::contains in C++20
    if (contains(get_ignored_functions(), FD->getNameAsString()))
      return true;

    ExportFunction(FD, exportMacro);
    return true;
  }

  void SetSema(clang::Sema& sema) {
    this->sema = &sema;
  }

  // Copied from clang-tidy's LexerUtils.cpps
  std::pair<clang::Token, clang::SourceLocation>
    getPreviousTokenAndStart(clang::SourceLocation Location, bool SkipComments) {
    clang::Token Token;
    Token.setKind(clang::tok::unknown);

    Location = Location.getLocWithOffset(-1);
    if (Location.isInvalid())
      return { Token, Location };

    auto StartOfFile = source_manager_.getLocForStartOfFile(source_manager_.getFileID(Location));
    while (Location != StartOfFile) {
      Location = clang::Lexer::GetBeginningOfToken(Location, source_manager_, context_.getLangOpts());
      if (!clang::Lexer::getRawToken(Location, Token, source_manager_, context_.getLangOpts()) &&
        (!SkipComments || !Token.is(clang::tok::comment))) {
        break;
      }
      Location = Location.getLocWithOffset(-1);
    }
    return { Token, Location };
  }

  clang::Token getPreviousToken(clang::SourceLocation Location, bool SkipComments) {
    auto [Token, Start] =
      getPreviousTokenAndStart(Location, SkipComments);
    return Token;
  }

  bool TryGetNestedNameStartLoc(clang::Decl *D, clang::SourceLocation &nameStart) {
    auto *TD = clang::dyn_cast<clang::TagDecl>(D);
    if (!TD)
      return false;

    auto *nestedName = TD->getQualifier();
    auto fullloc = GetFileLocation(TD->getQualifierLoc().getBeginLoc());
    
    auto SemanticDC = D->getDeclContext();
    auto lexialCtx = D->getLexicalDeclContext();

    // Check edge case of no nested name value, but the template still has a namespace prefix.
    // We can infer it from the semantic Delcontext being different to the Semantic one DC.
    // The LexicalDC will probably be a global namespace
    if (!nestedName && SemanticDC != lexialCtx && SemanticDC->isNamespace()) {
     auto tokenAndLoc = getPreviousTokenAndStart(TD->clang::NamedDecl::getLocation(), true);
     if (tokenAndLoc.first.getKind() == clang::tok::coloncolon) {
       std::pair<clang::Token, clang::SourceLocation> prev = tokenAndLoc;
       do {
         tokenAndLoc = prev;
         prev = getPreviousTokenAndStart(tokenAndLoc.second.getLocWithOffset(-1), true);
       } while ((prev.first.getKind() == clang::tok::raw_identifier && tokenAndLoc.first.getKind() == clang::tok::coloncolon) ||
                (prev.first.getKind() == clang::tok::coloncolon && tokenAndLoc.first.getKind() == clang::tok::raw_identifier));

       if (tokenAndLoc.first.getKind() != clang::tok::raw_identifier)
         return false;
       nameStart = tokenAndLoc.second;
       return true;
     }
    }

    // Check if the decl name is prefixed with a type or namespace 
    if (!nestedName)
      return false;

    // Use namespace prefix starting location to insert at instead of the 
    // normal class name location that skips the name prefixes
    nameStart = TD->getQualifierLoc().getBeginLoc();
    return true;
  }

  clang::VarDecl *FirstUnexportedStaticField(clang::CXXRecordDecl *D) {
    for (clang::Decl* FD : D->decls()) {
      clang::VarDecl *VD = clang::dyn_cast<clang::VarDecl>(FD);
      if (!VD)
        continue;
      if (!VD->isStaticDataMember() || VD->hasInit())
        continue;

      if (!isAlreadyExported(VD, false))
        return VD;
    }
    return NULL;
  }

  enum class UnexportedStatus {
    None = 0,
    Partial,
    All,
  };

  struct ExportedStats {
    int UnexportedMethods = 0;
    int UnexportedVariables = 0;
    int ExportedMethods = 0;
    int ExportedVariables = 0;
  };

  UnexportedStatus GetUnexportedMembers(clang::CXXRecordDecl *D, llvm::SmallVector<clang::Decl*> &unexportedMembers,
                                        ExportedStats &stats) {

    for (clang::Decl* FD : D->decls()) {
      clang::VarDecl *VD = clang::dyn_cast<clang::VarDecl>(FD);

      if (isAlreadyExported(FD, false)) {
        if (VD) {
          stats.ExportedVariables++;
        } else {
          stats.ExportedMethods++;
        }
        continue;
      }

      if (VD) {
        if (VD->isStaticDataMember() && !VD->hasInit()) {
          stats.UnexportedVariables++;
          unexportedMembers.push_back(FD);
        }
      }

      const clang::CXXMethodDecl* MD = clang::dyn_cast<clang::CXXMethodDecl>(FD);
      if (MD) {
        if (MD->isImplicit() || MD->isDeleted() || MD->isDefaulted() || MD->isPureVirtual())
          continue;

        if (MD->hasBody())
          continue;

        const clang::FunctionDecl *def = MD->getDefinition();
        if (MD->isInlined() || (def && def->isInlined())) {
          continue;
        }
        stats.UnexportedMethods++;
        unexportedMembers.push_back(FD);
      }
    }

    if (unexportedMembers.empty())
      return UnexportedStatus::None;
    
    if ((stats.ExportedMethods > 0 || stats.ExportedVariables > 0) && 
        (stats.UnexportedMethods > 0 || stats.UnexportedVariables > 0))
      return UnexportedStatus::Partial;

    return UnexportedStatus::All;
  }

  bool isAlreadyExported(const clang::Decl *D, bool ignoreInherited) {
    for (auto *Atrr : D->attrs()) {
      if (auto *annotation = clang::dyn_cast<clang::AnnotateAttr>(Atrr)) {
        if (Atrr->isInherited() && ignoreInherited) {
          if (debuglog) {
            llvm::StringRef name = D->getDeclKindName();
            if (auto *namedDecl = clang::dyn_cast<clang::NamedDecl>(D))
              name = namedDecl->getName();
            llvm::outs() << "Ignored inherited export attribute on: " << name << '\n';
          }
          continue;
        }

        if (annotation->getAnnotation() == "idt_export") {
          if (debuglog) {
            llvm::StringRef name = D->getDeclKindName();
            llvm::outs() << "Skipped '" << name << "' that already has export macro\n";
          }
          return true;
        }
      } else if (clang::isa<clang::DLLExportAttr>(Atrr) || clang::isa<clang::DLLImportAttr>(Atrr) ||
                 clang::isa<clang::VisibilityAttr>(Atrr)) {
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

  bool isLocationIgnored(clang::FullSourceLoc loc, clang::Decl::Kind nodeKind) {
    if (source_manager_.isInSystemHeader(loc))
      return true;

    if (mainfileonly && !isInsideMainFile(loc))
      return true;

    if (!isMainFileAHeader) {
      switch (nodeKind) {
      case clang::Decl::Var:
      case clang::Decl::ClassTemplateSpecialization:
        return false;
      default:
        return true;
      }
    }

    return false;
  }

  bool isInsideMainFile(clang::SourceLocation Loc) {
    if (!Loc.isValid())
      return false;
    clang::FileID FID = source_manager_.getFileID(source_manager_.getExpansionLoc(Loc));
    return FID == source_manager_.getMainFileID() || FID == source_manager_.getPreambleFileID();
  }

  llvm::StringRef GetLocationFilePath(clang::SourceLocation loc) {
    return context_.getFullLoc(loc).getFileEntryRef()->getName();;
  }

  struct FileLoc {
    llvm::StringRef path;
    unsigned line;
    unsigned column;

    FileLoc(const llvm::StringRef &path, unsigned line, unsigned column)
      : path(path), line(line), column(column) {
    }
  };

  FileLoc GetFileLocation(clang::SourceLocation loc) {
    if (loc.isInvalid())
      return FileLoc("", 0, 0);
    clang::FullSourceLoc location = context_.getFullLoc(loc).getExpansionLoc();
    return FileLoc(location.getFileEntryRef()->getName(), location.getLineNumber(), location.getColumnNumber());
  }

  clang::SourceRange GetLineSourceRangeLoc(clang::SourceLocation loc, int &offsetInLine) {
    clang::FullSourceLoc fullLoc = context_.getFullLoc(loc);
    auto &SM = context_.getSourceManager();
    int lineStart = fullLoc.getLineNumber();
    auto start = SM.translateLineCol(fullLoc.getFileID(), lineStart-1, 0).getLocWithOffset(1);
    auto end = SM.translateLineCol(fullLoc.getFileID(), lineStart, 0);
    offsetInLine = fullLoc.getColumnNumber();
    return clang::SourceRange(start, end);
  }

  llvm::StringRef GetSourceTextForRange(clang::SourceRange range) {
    return clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(range),
                                context_.getSourceManager(), context_.getLangOpts());
  }

  clang::FullSourceLoc GetFullExpansionLoc(clang::SourceLocation loc) {
     return context_.getFullLoc(loc).getExpansionLoc();
  }

};

class RewritesReceiver : public clang::edit::EditsReceiver {
  clang::Rewriter &Rewrite;

public:
  RewritesReceiver(clang::Rewriter &Rewrite) : Rewrite(Rewrite) {}

  void insert(clang::SourceLocation loc, clang::StringRef text) override {
    Rewrite.InsertText(loc, text);
  }

  void replace(clang::CharSourceRange range, clang::StringRef text) override {
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
  ResultCollector &owner;
  fixit_options options_;
  BaseExportOptions &exportOption;
  std::unique_ptr<clang::FixItRewriter2> rewriter_;

public:
  explicit consumer(clang::ASTContext &context, BaseExportOptions &exportOptions, bool skipFunctionBodies, ResultCollector &owner)
      : visitor_(context, exportOptions, skipFunctionBodies), owner(owner), exportOption(exportOptions) {}

  void HandleTranslationUnit(clang::ASTContext &context) override {
    using namespace clang::tooling;
    clang::SourceManager& SM = context.getSourceManager();

    if (apply_fixits) {
      clang::DiagnosticsEngine &diagnostics_engine = context.getDiagnostics();
      rewriter_ =
          std::make_unique<clang::FixItRewriter2>(diagnostics_engine,
                                                 SM,
                                                 context.getLangOpts(),
                                                 &options_);
      diagnostics_engine.setClient(rewriter_.get(), /*ShouldOwnClient=*/false);
    }

    visitor_.TraverseDecl(context.getTranslationUnitDecl());

    if (apply_fixits) {
      rewriter_->ProcessRewrites();

      clang::tooling::ApplyChangesSpec spec;
      auto formatStyle = exportOption.Owner->getClangFormatStyle();
      if (formatStyle) {
        spec.Style = *formatStyle;
        spec.Format = clang::tooling::ApplyChangesSpec::kAll;
      }

      for (auto I = rewriter_->buffer_begin(), 
                E = rewriter_->buffer_end(); I != E; I++) {
        clang::OptionalFileEntryRef Entry = SM.getFileEntryRefForID(I->first);

        llvm::SmallString<255> name = Entry->getName();
        llvm::sys::path::native(name);
        
        if (!owner.hasResult(name.str().str())) {
          std::string output;
          llvm::raw_string_ostream OS(output);

          clang::RewriteBuffer &RewriteBuf = I->second;
          RewriteBuf.write(OS);
          OS.flush();
          if (exportOption.AddExportHeaderInclude && !exportOption.ExportMacroHeader.empty()) {
            AtomicChange change(name, "add export header include");
            change.addHeader(exportOption.ExportMacroHeader);
            auto result = applyAtomicChanges(name, output, change, spec);
            if (!result) {
              llvm::errs() << "Failed to generate header include file change" << llvm::toString(result.takeError());
            }
            output = result.get();
          }
          owner.addResult(name.str().str(), output);
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
  FileOptionLookup& fileExportOptions;
  ResultCollector& owner;

  action(ResultCollector& owner)
    : owner(owner), fileExportOptions(fileExportOptions) {

  }

  BaseExportOptions *exportOptions;

#define DEFINE_EXPORRT_MACRO(fieldName, optName) \
    if (!exportOptions->fieldName.empty() && !seenMacros.contains(exportOptions->fieldName)) { \
      seenMacros.insert(exportOptions->fieldName);\
      PreproOpts.addMacroDef((exportOptions->fieldName + annotate).str()); \
    } \

  bool BeginInvocation(clang::CompilerInstance &CI) override {
    auto name = getCurrentFileOrBufferName();
    auto errorOrFile = CI.getFileManager().getFileRef(name);

    if (errorOrFile) {
      exportOptions = owner.getFileExportOptions(errorOrFile.get());
    } else {
      exportOptions = owner.getDefaultExportOptions();
    }

    auto &PreproOpts = CI.getPreprocessorOpts();

    llvm::StringSet<> seenMacros;
    llvm::StringRef annotate = "=__attribute__((annotate(\"idt_export\")))";
    PreproOpts.addMacroDef((exportOptions->ExportMacro + annotate).str());
    seenMacros.insert(exportOptions->ExportMacro);

    for (auto& name : exportOptions->OtherExportMacros) {
      PreproOpts.addMacroDef((name + annotate).str());
      seenMacros.insert(name);
    }

    if (!exportOptions->IsGeneratingMacro.empty()) {
      PreproOpts.addMacroDef(exportOptions->IsGeneratingMacro);
      seenMacros.insert(exportOptions->IsGeneratingMacro);
    }

    seenMacros.insert(exportOptions->ExportMacroHeader);
    OVERRIDABLE_OPTION_MACROS(DEFINE_EXPORRT_MACRO);
    return clang::ASTFrontendAction::BeginInvocation(CI);
  }

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef InFile) override {
    llvm::outs() << "Processing: " << InFile << '\n';
    CI.getPreprocessor().SetSuppressIncludeNotFoundError(true);

    bool skipFunctionBodies = true;
    CI.getFrontendOpts().SkipFunctionBodies = skipFunctionBodies;
    clang::DiagnosticsEngine &Diag = getCompilerInstance().getDiagnostics();
    Diag.setSeverity(clang::diag::warn_unused_private_field, clang::diag::Severity::Ignored, clang::SourceLocation());
    Diag.setSeverity(clang::diag::warn_unused_function, clang::diag::Severity::Ignored, clang::SourceLocation());
    Diag.setSeverity(clang::diag::warn_unused_variable, clang::diag::Severity::Ignored, clang::SourceLocation());
    Diag.setSeverity(clang::diag::warn_unused_const_variable, clang::diag::Severity::Ignored, clang::SourceLocation());
    Diag.setSeverity(clang::diag::warn_unused_private_field, clang::diag::Severity::Ignored, clang::SourceLocation());
    Diag.setSeverity(clang::diag::warn_unneeded_internal_decl, clang::diag::Severity::Ignored, clang::SourceLocation());
    return std::make_unique<idt::consumer>(CI.getASTContext(), *exportOptions, skipFunctionBodies, owner);
  }
};

struct factory : public ResultCollector, clang::tooling::FrontendActionFactory {
public:
  factory(FileOptionLookup *exportOptions) 
    : ResultCollector(exportOptions) {
  }

private:
  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<idt::action>(*this);
  }
};
}

class MemCDB : public clang::tooling::CompilationDatabase {
public:
  MemCDB(clang::tooling::CompilationDatabase &db) 
    : DB(db) {
  }

  std::vector<clang::tooling::CompileCommand> getCompileCommands(clang::StringRef F) const override {
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
  using namespace llvm::sys::path;


  auto options =
      CommonOptionsParser::create(argc, const_cast<const char **>(argv),
                                  idt::category, llvm::cl::OneOrMore);

  if (!options) {
    llvm::logAllUnhandledErrors(std::move(options.takeError()), llvm::errs());
    return EXIT_FAILURE;
  }
  baseOptions.IgnoredFiles = ignored_headers;

  if (externc_export_macro.getValue() != "") {
    baseOptions.ExportExternC = true;
  }

  if (!BuildIgnoredCXXRecordNames()) {
    return 1;
  }

  auto InferedDB = inferMissingCompileCommands(std::make_unique<MemCDB>(options->getCompilations()));

  if (mainfileonly.getNumOccurrences() == 0) {
    mainfileonly = true;
  }

  ExportOptions exportOptions;
  FileOptionLookup fileOptions;

  std::vector<std::string> sourcePathList;
  bool singleFile = true;
  if (compdb_pathmatch.getNumOccurrences() != 0) {
    singleFile = false;
    llvm::SmallVector<llvm::GlobPattern> pathPatterns;
    for (const auto& P : compdb_pathmatch) {
      auto Patten = llvm::GlobPattern::create(P);
      if (!Patten) {
        llvm::errs() << "Bad compilation database path glob " << P << ", patten was\"" << Patten.takeError() << "\"\n";
        return EXIT_FAILURE;
      } else {
        pathPatterns.push_back(std::move(Patten.get()));
      }
    }

    for (const auto &file : InferedDB->getAllFiles()) {
      for (auto &pat : pathPatterns) {
        if (pat.match(file)) {
          sourcePathList.push_back(file);
          break;
        }
      }
    }

    if (sourcePathList.empty()) {
      llvm::errs() << "No paths matched by compilation database path globs\n";
      return EXIT_FAILURE;
    }

  } else if (!root_header_directory.empty()) {
    singleFile = false;

    llvm::Error error = llvm::Error::success();
    if (export_config_path.empty()) {
      error = ExportOptions::loadFromDirectory(root_header_directory, exportOptions);
    } else if(ExportOptions::directoryHasExportConfig(root_header_directory)) {
      error = ExportOptions::loadFromFile(export_config_path, exportOptions);
      if(!error)
        exportOptions.setRootDirectory(root_header_directory);
    }

    if (error) {
      llvm::errs() << "Reading export_config.json failed: " << error;
      return EXIT_FAILURE;
    }
    exportOptions.setOverridesAndDefaults(baseOptions);
    
    llvm::SmallString<256> headerDirectory;
    std::vector<std::string> files;

    if (!export_group_to_run.empty()) {
      llvm::outs() << "Restricting generation to export config group \"" << export_group_to_run << "\"\n";
      for (auto& group : exportOptions.getGroups()) {
        group.Disabled = llvm::StringRef(group.Name).compare_insensitive(export_group_to_run) != 0;
      }
    }

    if (auto err = exportOptions.gatherAllFiles(sourcePathList, fileOptions)) {
      llvm::errs() << err;
      return EXIT_FAILURE;
    }
    
   // if (!GatherHeaders(options.get(), sourcePathList)) {
    //  return EXIT_FAILURE;
   // }
  }

  int result;
  idt::factory factory(&fileOptions);
  ClangToolRunner runner;
  llvm::Error err = runner.runTool(*InferedDB.get(), factory, sourcePathList, thread_parallelism);

  // If the user has enabled interactive mode list the files that will be modified
  if (interactive && factory.hasResults() && !err) {
    llvm::outs() << "Files to modify:\n";
    for (auto & pair : factory.results()) {
      llvm::StringRef filename = pair.first;
      llvm::outs() << "  " << filename << '\n';
    }
  }

  // List failing files after we listed all the files that wil be modified so there easier to find
  if (runner.hasErrors()) {
    llvm::errs() << "Failing files:\n";
    for (auto & file : runner.getFailingFiles()) {
      llvm::errs() << "  " << file << "\n";
    }
  }

  if (err) {
    llvm::errs() << "Running ASTAction failed:" << err;
    return EXIT_FAILURE;
  }

  if (!apply_fixits) {
    return result;
  }

  // If the user has enabled interactive mode ask before committing buffered changes
  if (interactive && factory.hasResults()) {
    char c;
    llvm::outs() << "Commit changes Yes, No?\n";
    while (true) {
      if (!std::cin.get(c)) {
        return 1;
      }

      if (c == 'y')
        break;

      if (c != 'n')
        return 1;
    }
  }

  if (apply_fixits) {
    for (auto & pair : factory.results()) {
      std::error_code EC;
      std::unique_ptr<llvm::raw_fd_ostream> OS;
      llvm::StringRef filename = pair.first;
      OS.reset(new llvm::raw_fd_ostream(filename, EC, llvm::sys::fs::OF_None));
      if (EC) {
        llvm::errs() << "Failed to write changes for '" << filename << "', Unable to open file error was " << EC.message();
        result = 1;
        continue;
      }
      OS->write(pair.second.c_str(), pair.second.size());
      OS->flush();
    }

    if (factory.hasResults()) {
      llvm::outs() << "All file changes written.\n";
    }
  }

  return result;
}
