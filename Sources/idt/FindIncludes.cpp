// Copyright (c) 2024 Thomas Fransham.  All Rights Reserved.
// SPDX-License-Identifier: BSD-3-Clause

#include "FindIncludes.h"
#include "ExportOptionsConfig.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/TextDiagnostic.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/ThreadPool.h"

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

Error GatherFilesInDirectory(StringRef directory, std::vector<std::string> &foundfiles, std::function<PathChecker> Filter) {
  using namespace llvm::sys::fs;
  using namespace llvm::sys;
  std::error_code ec;

  if (!is_directory(directory)) {
    return createStringError("Header directory \"" + directory + "\" does not exist\n");
  }

  SmallString<256> normalizedPath;
  llvm::sys::path::native(directory, normalizedPath);

  if (!llvm::sys::path::is_separator(normalizedPath[normalizedPath.size()])) {
    normalizedPath += llvm::sys::path::get_separator();
  }
  int prefixSize = normalizedPath.size();

  for (recursive_directory_iterator F(directory, ec), E; F != E; F.increment(ec)) {
    if (ec) {
      return createStringError(ec, "Directory iteration failed when trying to find headers");
    }

    if (F->type() != file_type::regular_file)
      continue;

    StringRef path = F->path();

    normalizedPath = path; ;

    if (Filter(normalizedPath.substr(prefixSize))) {
      llvm::outs() << "Skipped ignored file: " << path << "\n";
      continue;
    }

    foundfiles.push_back(normalizedPath.str().str());
  }
  return Error::success();
}

Error GatherDirsInDirectory(StringRef directory, std::vector<std::string> &FoundDirectories, HeaderPathMatcher *filter) {
  using namespace llvm::sys::fs;
  using namespace llvm::sys;
  std::error_code ec;

  if (!is_directory(directory)) {
    return createStringError("Header directory \"" + directory + "\" does not exist\n");
  }

  SmallString<256> normalizedPath;
  llvm::sys::path::native(directory, normalizedPath);
  int prefixSize = normalizedPath.size();

  for (directory_iterator F(directory, ec), E; F != E; F.increment(ec)) {
    if (ec) {
      return createStringError(ec, "Directory iteration failed when trying to find headers");
    }

    if (F->type() != file_type::directory_file)
      continue;

    StringRef path = F->path();

    normalizedPath = path; ;
    std::replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');

    if (filter && filter->match(normalizedPath.substr(prefixSize + 1))) {
      llvm::outs() << "Skipped ignored directory: " << path << "\n";
      continue;
    }

    FoundDirectories.push_back(normalizedPath.str().str());
  }
  return Error::success();
}

Error GatherHeaders(StringRef headerDirectory, tooling::CommonOptionsParser &options, std::vector<std::string>& rootheaders) {
  using namespace llvm::sys::fs;
  using namespace clang::tooling;

  std::vector<std::string> files;

  SmallString<256> NativePath;
  llvm::sys::path::native(headerDirectory, NativePath);

  auto Filter = [](StringRef path) {
    if (!isHeaderFile(path)) {
      llvm::outs() << "Skipped non header file: " << path << "\n";
      return true;
    }
    return false;
  };

  if (Error err = GatherFilesInDirectory(NativePath, files, Filter)) {
    return err;
  }

  return Error::success();
}

 ClangToolRunner::ClangToolRunner() 
   : StopOnFirstError(false), PrintProgress(true) {
   bool ShowColors = true;
   if (std::optional<std::string> NoColor =
     llvm::sys::Process::GetEnv("NO_COLOR");
     NoColor && !NoColor->empty()) {
     // If the user set the NO_COLOR environment variable, we'll honor that
     // unless the command line overrides it.
     ShowColors = false;
   }

   DiagOptions = new DiagnosticOptions();

   if (ShowColors) {
     DiagOptions->ShowColors = ShowColors;
     DiagOptions->UseANSIEscapeCodes = true;
   }
 }

ClangToolRunner::~ClangToolRunner() {}

void ClangToolRunner::Log(llvm::Twine Msg) {
  std::unique_lock<std::mutex> LockGuard(TUMutex);
  llvm::outs() << Msg.str() << "\n";
}

void ClangToolRunner::AppendError(llvm::Twine Msg) {
  std::unique_lock<std::mutex> LockGuard(TUMutex);
  llvm::errs() << Msg.str() << "\n";
}

class WrapperFactory : public FrontendActionFactory {
public:
  WrapperFactory(ClangToolRunner &Owner, FrontendActionFactory &Factory)
    : Owner(Owner), Factory(Factory), DiagBuffer(&Owner){
  }

  std::unique_ptr<FrontendAction> create() override {
    //return std::make_unique<OutputCapturingFrontendAction>(Owner, std::move(Factory.create()), DiagBuffer);
    return Factory.create();
  }

  bool runInvocation(std::shared_ptr<CompilerInvocation> Invocation,
                     FileManager *Files,
                     std::shared_ptr<PCHContainerOperations> PCHContainerOps,
                     DiagnosticConsumer *DiagConsumer) override {

    // Create a compiler instance to handle the actual work.
    CompilerInstance Compiler(std::move(PCHContainerOps));
    Compiler.setInvocation(std::move(Invocation));
    Compiler.setFileManager(Files);

    // The FrontendAction can have lifetime requirements for Compiler or its
    // members, and we need to ensure it's deleted earlier than Compiler. So we
    // pass it to an std::unique_ptr declared after the Compiler variable.
    std::unique_ptr<FrontendAction> ScopedToolAction(create());
    DiagBuffer.setCurrentAction(ScopedToolAction.get());

    // Create the compiler's actual diagnostics engine.
    // &DiagBuffer
    Compiler.createDiagnostics(&DiagBuffer, /*ShouldOwnClient=*/false);
    if (!Compiler.hasDiagnostics())
      return false;

    Compiler.createSourceManager(*Files);
    const bool Success = Compiler.ExecuteAction(*ScopedToolAction);

    Files->clearStatCache();
    return Success;

    // Note we skip calling Factory's runInvocation so our 'create' function is called
    //return FrontendActionFactory::runInvocation(Invocation, Files, PCHContainerOps,
    //                             &DiagBuffer);
  }

  FrontendAction* Action;
  ClangToolRunner &Owner;
  FrontendActionFactory &Factory;
  BufferedDiagnostics DiagBuffer;
};

Error ClangToolRunner::runTool(CompilationDatabase &CompDb,
                               FrontendActionFactory &ActionFactory,
                               std::vector<std::string> Files,
                               int threadCount) {
  Factory = &ActionFactory;
  TotalFiles = Files.size();
  Compilations = &CompDb;
  ItemsProcessed = 0;
  ErrorMsg.clear();

  {
    llvm::DefaultThreadPool Pool(llvm::hardware_concurrency(threadCount));
    for (std::string &File : Files) {
      Pool.async([&](std::string &path) { processFile(path); }, File);
    }
    // Make sure all tasks have finished before resetting the working directory.
    Pool.wait();
  }

  if (!ErrorMsg.empty())
    return createStringError(ErrorMsg);

  return Error::success();
}

void ClangToolRunner::processFile(const std::string &Path) {
  if (StopOnFirstError && hasErrors())
    return;
  int jobId = ItemsProcessed++;
  if (PrintProgress)
    Log("[" + std::to_string(jobId) + "/" +
        std::to_string(TotalFiles) + "] Processing file " + Path);

  // Each thread gets an independent copy of a VFS to allow different
  // concurrent working directories.
  IntrusiveRefCntPtr<llvm::vfs::FileSystem> FS =
      llvm::vfs::createPhysicalFileSystem();
  
  ClangTool Tool(*Compilations, {Path},
                 std::make_shared<PCHContainerOperations>(), FS);
  auto commands = Compilations->getCompileCommands(Path);
  // Tool.appendArgumentsAdjuster(Action.second);
  // Tool.appendArgumentsAdjuster(getDefaultArgumentsAdjusters());

  WrapperFactory factory(*this, *Factory);

  if (Tool.run(&factory))
    AppendError(llvm::Twine("Failed to run action on ") + Path + "\n");
}

void ClangToolRunner::logDiagnostics(BufferedDiagnostics &buffer) {
  if (buffer.begin() != buffer.end()) {
    std::unique_lock<std::mutex> LockGuard(TUMutex);
    if (buffer.getNumErrors() > 0) {
      FailingFiles.push_back(buffer.GetCurrentFile().str());
    }
    buffer.PrintDiagnostic(*DiagOptions);
  }
}

std::mutex &ClangToolRunner::getMutex() { return TUMutex; }

bool ClangToolRunner::hasErrors() { 
  return !FailingFiles.empty(); 
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
        if (!ErrorMsg.empty()) {
          return;
        }
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

  Error err = runClangToolMultithreaded(options.getCompilations(), *factory.get(), files, - 1);
  if (err)
    return err;

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

bool HeaderPathMatcher::match(llvm::StringRef path) {
  for (auto& pat : pattens) {
    if (pat.match(path))
      return true;
  }

  for (auto& s : PlainStrings) {
    bool result;
    switch (s.first) {
    case PathMatchMode::Start:
      result = path.starts_with_insensitive(s.second);
      break;
    case PathMatchMode::End:
      result = path.ends_with_insensitive(s.second);
      break;
    case PathMatchMode::Anywhere:
      result = path.contains_insensitive(s.second);
      break;
    }

    if (result) {
      return true;
    }
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

Error HeaderPathMatcher::addPaths(std::vector<std::string> &pathGlobs, PathMatchMode MatchMode) {
  for (const auto& path : pathGlobs) {
    if(auto Err = addPath(path, MatchMode))
      return Err;
  }
  return Error::success();
}

llvm::Error HeaderPathMatcher::addDirectoryRoots(std::vector<std::string> &Directories) {
  llvm::SmallString<256> Patten;

  for (auto &path : Directories) {
    Patten.clear();
    Patten = path;
    // Make sure the path ends in a path separator so we don't partially match a directory name
    if (Patten.front() == '\\') {
      Patten[Patten.size() - 1] = '/';
    } else if (Patten.front() != '\\') {
      Patten += '/';
    }

    if (auto Err = addPath(Patten, PathMatchMode::Start))
      return Err;
  }

  return Error::success();
}

Error HeaderPathMatcher::addPath(StringRef Path, PathMatchMode MatchMode) {
  llvm::SmallString<256> normalizedPath;

  auto pattenStart = Path.find_first_of("?*[{\\");

  if (pattenStart == std::string::npos) {
    normalizedPath = Path;
    sys::path::native(normalizedPath);
    PlainStrings.emplace_back(std::make_pair(MatchMode, normalizedPath.str().str()));
  } else {
  // Normalize slashes in patten to how paths converted by sys::path::native are set
#if defined(_WIN32)
    size_t curr = 0;
    size_t Found = Path.find('/', curr);

    while (Found != StringRef::npos) {
      normalizedPath.append(Path.substr(curr, Found - curr));
      normalizedPath.append("\\\\");
      curr = Found + 1;
      Found = Path.find('/', curr);
    }
#else
    normalizedPath = Path;
    sys::path::native(normalizedPath);
#endif

    auto err = addPathPatten(normalizedPath);
    if (err)
      return err;
  }

  return Error::success();
}

Error HeaderPathMatcher::addPathPatten(llvm::StringRef pathGlob) {
  auto Patten = llvm::GlobPattern::create(pathGlob);
  if (!Patten)
    return createStringError("Bad header path glob '" + pathGlob + "', " + toString(Patten.takeError()));

  pattens.push_back(std::move(Patten.get()));
  return Error::success();
}

void HeaderPathMatcher::addPlainPath(llvm::StringRef Path, PathMatchMode MatchMode) {
  PlainStrings.emplace_back(std::make_pair(MatchMode, Path));
}

void BufferedDiagnostics::BeginSourceFile(const clang::LangOptions &LangOpts,
                                          const clang::Preprocessor *PP) {
  this->LangOpts = LangOpts;
  this->PP = PP;
}


void BufferedDiagnostics::EndSourceFile() { 
  Owner->logDiagnostics(*this); 
  Out.clear();
}

clang::StringRef BufferedDiagnostics::GetCurrentFile() {
  return CurrentAction ? CurrentAction->getCurrentFile() : "";
}

void BufferedDiagnostics::HandleDiagnostic(
    clang::DiagnosticsEngine::Level DiagLevel, const clang::Diagnostic &Info) {
  Out.emplace_back(DiagLevel, Info);
  DiagnosticConsumer::HandleDiagnostic(DiagLevel, Info);
}

void BufferedDiagnostics::PrintDiagnostic(DiagnosticOptions &Options) {
  TextDiagnostic Renderer(llvm::outs(), LangOpts, &Options, PP);

  for (auto& diag : Out) {
    Renderer.emitStoredDiagnostic(diag);
  }
}

bool HasPotentialExports(MemoryBuffer &file) {
  StringRef buffer = file.getBuffer();
  return buffer.contains("extern") || buffer.contains("cl::opt") || buffer.contains("template");
}

Error SoftFilterPotentialExports(std::vector<std::string>& PathList, int ThreadCount) {
  std::mutex ResultMutex;
  std::vector<std::error_code> ErrorList;
  std::vector<std::string> HasExportables;

  {
    llvm::DefaultThreadPool Pool(llvm::hardware_concurrency(ThreadCount));
    for (std::string File : PathList) {
      Pool.async(
        [&](std::string& Path) {
          auto buffer = MemoryBuffer::getFile(Path, false, false, false);
          if (!buffer) {
            std::unique_lock<std::mutex> LockGuard(ResultMutex);
            ErrorList.emplace_back(buffer.getError());
            llvm::errs() << "Failed to open " << Path << " to check for missing exports: " << buffer.getError().message() << "\n";
            return;
          } else {
            std::unique_lock<std::mutex> LockGuard(ResultMutex);
            llvm::outs() << "Scanning: " << Path << " for potential exports\n";
          }

          if (HasPotentialExports(*buffer.get())) {
            std::unique_lock<std::mutex> LockGuard(ResultMutex);
            HasExportables.emplace_back(Path);
          }
      }, File);
    }
    // Make sure all tasks have finished before resetting the working directory.
    Pool.wait();
  }

  if (!ErrorList.empty()) {
    return createStringError("Scanning source files failed, " + ErrorList.front().message());
  }

  PathList = HasExportables;
  return Error::success();
}
