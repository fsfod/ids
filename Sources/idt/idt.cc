// Copyright (c) 2021 Saleem Abdulrasool.  All Rights Reserved.
// SPDX-License-Identifier: BSD-3-Clause

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"

#include <cstdlib>
#include <iostream>
#include <set>
#include <string>

namespace idt {
llvm::cl::OptionCategory category{"interface definition scanner options"};
}

namespace {

llvm::cl::opt<std::string>
export_macro("export-macro",
             llvm::cl::desc("The macro to decorate interfaces with"),
             llvm::cl::value_desc("define"), llvm::cl::Required,
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

namespace idt {
class visitor : public clang::RecursiveASTVisitor<visitor> {
  clang::ASTContext &context_;
  clang::SourceManager &source_manager_;
  clang::Sema *sema;

  clang::DiagnosticBuilder
  unexported_public_interface(clang::SourceLocation location) {
    clang::DiagnosticsEngine &diagnostics_engine = context_.getDiagnostics();

    static unsigned kID =
        diagnostics_engine.getCustomDiagID(clang::DiagnosticsEngine::Remark,
                                           "unexported public interface %0");

    return diagnostics_engine.Report(location, kID);
  }

  clang::DiagnosticBuilder
  exported_private_interface(clang::SourceLocation location) {
    clang::DiagnosticsEngine &diagnostics_engine = context_.getDiagnostics();

    static unsigned kID =
        diagnostics_engine.getCustomDiagID(clang::DiagnosticsEngine::Remark,
                                           "exported private interface %0");

    return diagnostics_engine.Report(location, kID);
  }

  template <typename Decl_>
  inline clang::FullSourceLoc get_location(const Decl_ *TD) const {
    return context_.getFullLoc(TD->getBeginLoc()).getExpansionLoc();
  }

public:
  explicit visitor(clang::ASTContext &context)
      : context_(context), source_manager_(context.getSourceManager()), sema(nullptr) {}

  bool VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    clang::FullSourceLoc location = get_location(D);

    // Ignore declarations from the system.
    if (source_manager_.isInSystemHeader(location))
      return true;

    if (D->isImplicit() || !D->isThisDeclarationADefinition()) {
      return true;
    }

    if (D->hasAttr<clang::DLLExportAttr>() ||
        D->hasAttr<clang::DLLImportAttr>())
      return true;

    if (!isInsideMainFile(D->getLocation()))
      return true;

    if (llvm::isa<clang::ClassTemplateSpecializationDecl>(D)) {
      if (!D->hasExternalLexicalStorage()) {
        return true;
      }
      location = context_.getFullLoc(D->getLocation()).getExpansionLoc();
    }

    if (D->isClass() || D->isStruct()) {
      //D->dump();
      clang::SourceLocation insertion_point1 = D->getLocation();
      auto aroucerange = D->getSourceRange();
      clang::SourceLocation insertion_point = D->getSourceRange().getEnd();// D->getDeclName().getAsIdentifierInfo();

      location = context_.getFullLoc(D->getLocation()).getExpansionLoc();

      if (D->needsImplicitCopyConstructor() || D->needsImplicitCopyAssignment() || D->needsImplicitMoveAssignment()) {
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
          export_macro + " ");
      return true;
    }
  }


  bool isInsideMainFile(clang::SourceLocation Loc) {
    if (!Loc.isValid())
      return false;
    clang::FileID FID = source_manager_.getFileID(source_manager_.getExpansionLoc(Loc));
    return FID == source_manager_.getMainFileID() || FID == source_manager_.getPreambleFileID();
  }


  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    clang::FullSourceLoc location = get_location(FD);

    // Ignore declarations from the system.
    if (source_manager_.isInSystemHeader(location))
      return true;

    // We are only interested in non-dependent types.
    if (FD->isDependentContext())
      return true;

    // If the function has a body, it can be materialized by the user.
    if (FD->hasBody())
      return true;

    // Ignore friend declarations.
    if (llvm::isa<clang::FriendDecl>(FD))
      return true;

    // Ignore deleted and defaulted functions (e.g. operators).
    if (FD->isDeleted() || FD->isDefaulted())
      return true;

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

    // If the function has a dll-interface, it is properly annotated.
    // TODO(compnerd) this should also handle `__visibility__`
    if (FD->hasAttr<clang::DLLExportAttr>() ||
        FD->hasAttr<clang::DLLImportAttr>())
      return true;

    // Ignore known forward declarations (builtins)
    // TODO(compnerd) replace with std::set::contains in C++20
    if (contains(get_ignored_functions(), FD->getNameAsString()))
      return true;

    clang::SourceLocation insertion_point =
        FD->getTemplatedKind() == clang::FunctionDecl::TK_NonTemplate
            ? FD->getBeginLoc()
            : FD->getInnerLocStart();
    unexported_public_interface(location)
        << FD
        << clang::FixItHint::CreateInsertion(insertion_point,
                                             export_macro + " ");
    return true;
  }

  void SetSema(clang::Sema& sema) {
    this->sema = &sema;
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
  explicit consumer(clang::ASTContext &context)
      : visitor_(context) {}

  void HandleTranslationUnit(clang::ASTContext &context) override {
    if (apply_fixits) {
      clang::DiagnosticsEngine &diagnostics_engine = context.getDiagnostics();
      rewriter_ =
          std::make_unique<clang::FixItRewriter>(diagnostics_engine,
                                                 context.getSourceManager(),
                                                 context.getLangOpts(),
                                                 &options_);
      diagnostics_engine.setClient(rewriter_.get(), /*ShouldOwnClient=*/false);
    }

    visitor_.TraverseDecl(context.getTranslationUnitDecl());

    if (apply_fixits)
      rewriter_->WriteFixedFiles();
  }

  void InitializeSema(clang::Sema& sema) override {
    visitor_.SetSema(sema);
  }
};

struct action : clang::ASTFrontendAction {
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef) override {
    return std::make_unique<idt::consumer>(CI.getASTContext());
  }
};

struct factory : clang::tooling::FrontendActionFactory {
  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<idt::action>();
  }
};
}

int main(int argc, char *argv[]) {
  using namespace clang::tooling;

  auto options =
      CommonOptionsParser::create(argc, const_cast<const char **>(argv),
                                  idt::category, llvm::cl::OneOrMore);
  if (options) {
    ClangTool tool{options->getCompilations(), options->getSourcePathList()};
    return tool.run(new idt::factory{});
  } else {
    llvm::logAllUnhandledErrors(std::move(options.takeError()), llvm::errs());
    return EXIT_FAILURE;
  }
}
