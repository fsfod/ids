// Copyright (c) 2024 Thomas Fransham.  All Rights Reserved.
// SPDX-License-Identifier: BSD-3-Clause

#include "clang/Frontend/FrontendAction.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/ThreadPool.h"
#include "FindIncludes.h"
#include "ExportOptionsConfig.h"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;


class IncludeFinder : public clang::PPCallbacks {

public:
  explicit IncludeFinder(clang::Preprocessor *PP)
    : PP(PP), SM(PP->getSourceManager()) {
  }

  void InclusionDirective(SourceLocation HashLoc,
    const Token &IncludeTok, StringRef FileName,
    bool IsAngled, CharSourceRange FilenameRange,
    OptionalFileEntryRef File,
    StringRef SearchPath, StringRef RelativePath,
    const clang::Module *SuggestedModule,
    bool ModuleImported,
    SrcMgr::CharacteristicKind FileType) override {

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

 FindIncludesAction::FindIncludesAction(HeaderResults &includeList)
    : headerIncludes(includeList) {}

void FindIncludesAction::ExecuteAction() {
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
    // if (Tok.is(tok::annot_module_begin))
    //  Rewrite->handleModuleBegin(Tok);
  } while (Tok.isNot(tok::eof));
  // RewriteMacrosInInput(CI.getPreprocessor(), OS.get());

  auto &SM = PP.getSourceManager();
  const auto &file = SM.getFileEntryRefForID(SM.getMainFileID());

  headerIncludes.addResult(file->getName().str(), std::make_unique<StringSet<>>(std::move(includeFinders->IncludedHeaders)));
}

bool isHeaderFile(StringRef Path) {
  return StringSwitch<bool>(sys::path::extension(Path))
    .Cases(".h", ".H", ".hh", ".hpp", ".hxx", true)
    .Default(false);
}

Error GatherFilesInDirectory(StringRef directory, std::vector<std::string> &foundfiles, HeaderPathMatcher *filter) {
  using namespace llvm::sys::fs;
  using namespace llvm::sys;
  std::error_code ec;

  if (!is_directory(directory)) {
    return createStringError("Header directory \"" + directory + "\" does not exist\n");
  }

  SmallString<256> normalizedPath;
  llvm::sys::path::native(directory, normalizedPath);
  int prefixSize = normalizedPath.size();

  for (recursive_directory_iterator F(directory, ec), E; F != E; F.increment(ec)) {
    if (ec) {
      return createStringError(ec, "Directory iteration failed when trying to find headers");
    }

    if (F->type() != file_type::regular_file)
      continue;

    StringRef path = F->path();
    if (!isHeaderFile(path)) {
      llvm::outs() << "Skipped non header file: " << path << "\n";
      continue;
    }

    normalizedPath = path; ;
    std::replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');

    if (filter && filter->match(normalizedPath.substr(prefixSize + 1))) {
      llvm::outs() << "Skipped ignored header: " << path << "\n";
      continue;
    }

    foundfiles.push_back(normalizedPath.str().str());
  }
  return Error::success();
}

Error GatherHeaders(StringRef headerDirectory, tooling::CommonOptionsParser &options, std::vector<std::string>& rootheaders) {
  using namespace llvm::sys::fs;
  using namespace clang::tooling;

  std::vector<std::string> files;

  SmallString<256> NativePath;
  llvm::sys::path::native(headerDirectory, NativePath);
  if (Error err = GatherFilesInDirectory(NativePath, files, NULL)) {
    return err;
  }

  return Error::success();
}

Error runClangToolMultithreaded(CompilationDatabase &Compilations, FrontendActionFactory& factory, std::vector<std::string> Files, int threadCount) {
  std::string ErrorMsg;
  std::mutex TUMutex;
  auto AppendError = [&](llvm::Twine Err) {
    std::unique_lock<std::mutex> LockGuard(TUMutex);
    ErrorMsg += Err.str();
  };

  auto Log = [&](llvm::Twine Msg) {
    std::unique_lock<std::mutex> LockGuard(TUMutex);
    llvm::errs() << Msg.str() << "\n";
  };


  // Add a counter to track the progress.
  const std::string TotalNumStr = std::to_string(Files.size());
  unsigned Counter = 0;
  auto Count = [&]() {
    std::unique_lock<std::mutex> LockGuard(TUMutex);
    return ++Counter;
  };


  {
    llvm::DefaultThreadPool Pool(llvm::hardware_concurrency(threadCount));
    for (std::string File : Files) {
      Pool.async(
        [&](std::string Path) {
        Log("[" + std::to_string(Count()) + "/" + TotalNumStr +
          "] Processing file " + Path);
        // Each thread gets an independent copy of a VFS to allow different
        // concurrent working directories.
        IntrusiveRefCntPtr<llvm::vfs::FileSystem> FS =
          llvm::vfs::createPhysicalFileSystem();
        ClangTool Tool(Compilations, { Path },
          std::make_shared<PCHContainerOperations>(), FS);
       // Tool.appendArgumentsAdjuster(Action.second);
       // Tool.appendArgumentsAdjuster(getDefaultArgumentsAdjusters());

        if (Tool.run(&factory))
          AppendError(llvm::Twine("Failed to run action on ") + Path +
            "\n");
      },
        File);
    }
    // Make sure all tasks have finished before resetting the working directory.
    Pool.wait();
  }

  if (!ErrorMsg.empty())
    return createStringError(ErrorMsg);

  return Error::success();
}

Error getRootHeaders(tooling::CommonOptionsParser &options, std::vector<std::string> files, std::vector<std::string> &rootHeaders) {
  using namespace clang::tooling;

  auto factory = std::make_unique<FindIncludesFrontendActionFactory>();

  ClangTool tool2(options.getCompilations(), files);
  tool2.run(factory.get());

  runClangToolMultithreaded(options.getCompilations(), *factory.get(), files, - 1);

  llvm::StringSet<> nonroots;
  for (auto &header : factory->headerIncludes) {
    nonroots.insert(header.second->begin(), header.second->end());
  }

  for (auto &path : files) {
    if (!nonroots.contains(path)) {
      rootHeaders.push_back(path);
    }
  }

  return Error::success();
}


Expected<HeaderPathMatcher> HeaderPathMatcher::create(std::vector<std::string> &pathGlobs, const std::string &rootDirectory) {
  HeaderPathMatcher matcher;
  Error err = matcher.addPaths(pathGlobs, rootDirectory);
  if (err)
    return err;

  return matcher;
}

bool HeaderPathMatcher::match(llvm::StringRef path) {
  for (auto& pat : pattens) {
    if (pat.match(path))
      return true;
  }

  for (auto& s : PlainStrings) {
    if (path.rfind_insensitive(s) != StringRef::npos)
      return true;
  }
  
  return false;
}

std::vector<std::string> HeaderPathMatcher::filterPathList(std::vector<std::string>& paths) {
  llvm::SmallString<256> normalizedPath;
  std::vector<std::string> result(paths.size());

  for (auto& path : paths) {
    normalizedPath = path;
    std::replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
    if (!match(normalizedPath)) {
      result.push_back(path);
    }
  }
  return result;
}

Error HeaderPathMatcher::addPaths(std::vector<std::string>& pathGlobs, const std::string& rootDirectory) {
  llvm::SmallString<256> normalizedPath;

  for (const auto& path : pathGlobs) {
    normalizedPath = path;
    std::replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
    auto pattenStart = path.find_first_of("?*[{\\");

    if (pattenStart == std::string::npos) {
      PlainStrings.push_back(normalizedPath.str().str());
    } else {
      auto err = addRawPathPatten(normalizedPath);
      if (err)
        return err;
    }
  }
  return Error::success();
}

Error HeaderPathMatcher::addRawPathPatten(llvm::StringRef pathGlob) {
  auto Patten = llvm::GlobPattern::create(pathGlob);
  if (!Patten)
    return createStringError("Bad header path glob '" + pathGlob + "', " + toString(Patten.takeError()));

  pattens.push_back(std::move(Patten.get()));
  return Error::success();
}

