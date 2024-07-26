// Copyright (c) 2024 Thomas Fransham.  All Rights Reserved.
// SPDX-License-Identifier: BSD-3-Clause

#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Error.h"
#include "llvm/ADT/DenseMap.h"
#include <string>
#include <vector>

namespace clang {
  class TextDiagnosticBuffer;
}

typedef std::pair<std::string, llvm::StringSet<>> FileIncludeResults;

class HeaderPathMatcher;
template<typename KeyT, typename ValueT> class ThreadSafeToolResults;

class HeaderPathMatcher {
public:
  HeaderPathMatcher() = default;
  static llvm::Expected<HeaderPathMatcher> create(std::vector<std::string> &pathGlobs, const std::string &rootDirectory = "");
  llvm::Error addRawPathPatten(llvm::StringRef pathGlob);

  bool match(llvm::StringRef path);
  std::vector<std::string> filterPathList(std::vector<std::string>& paths);

private:
  llvm::Error addPaths(std::vector<std::string> &pathGlobs, const std::string &rootDirectory);
  llvm::SmallVector<llvm::GlobPattern> pattens;
  llvm::SmallVector<std::string> PlainStrings;
};

llvm::Error GatherFilesInDirectory(llvm::StringRef directory, std::vector<std::string> &foundFiles, HeaderPathMatcher *filter);

llvm::Error runClangToolMultithreaded(clang::tooling::CompilationDatabase &Compilations, clang::tooling::FrontendActionFactory& factory,
                            std::vector<std::string> Files, int threadCount = 0);

// Create a list of headers that if we start a translation unit from each would visit all the headers of the original list
llvm::Error getRootHeaders(clang::tooling::CommonOptionsParser & options, std::vector<std::string> files, std::vector<std::string>& rootHeaders);


typedef ThreadSafeToolResults<std::string, std::unique_ptr<llvm::StringSet<>>> HeaderResults;

template <typename KeyT, typename ValueT> class ThreadSafeToolResults {
  typedef ValueT(createFunc)(KeyT key);

public:
  ThreadSafeToolResults() {
    ValueCreator = [](KeyT) { return ValueT(); };
  }

  ThreadSafeToolResults(std::function<createFunc> valueCreator)
      : ValueCreator(valueCreator) {}

  void addResult(KeyT key, ValueT &&Value) {
    std::unique_lock<std::mutex> LockGuard(Mutex);
    Results[key] = std::move(Value);
  }

  void addResult(KeyT key, ValueT &Value) {
    std::unique_lock<std::mutex> LockGuard(Mutex);
    Results[key] = std::move(Value);
  }

  bool hasResult(KeyT key) {
    std::unique_lock<std::mutex> LockGuard(Mutex);
    return Results.find(key) != Results.end();
  }

  bool hasResults() {
    return !Results.empty();
  }

  template <typename U = ValueT> std::enable_if_t<std::is_same<U, std::unique_ptr<typename U::element_type>>::value, bool>
  tryClaimResult(KeyT key, ValueT *&value) {
    std::unique_lock<std::mutex> LockGuard(Mutex);
    if (Results.contains(key)) {
      value = nullptr;
      return false;
    } else {
      Results[key] = std::move(ValueCreator(key));
      value = Results[key].get();
      return true;
    }
  }

  typename llvm::DenseMap<KeyT, ValueT>::iterator begin() {
    return Results.begin();
  }

  typename llvm::DenseMap<KeyT, ValueT>::iterator end() {
    return Results.end();
  }

  typename const llvm::DenseMap<KeyT, ValueT> &results() {
    return Results;
  }

  void
  forEachResult(llvm::function_ref<void(KeyT Key, ValueT Value)> Callback) {
    Results.forEachResult(Callback);
  }

private:
  std::function<createFunc> ValueCreator;
  llvm::DenseMap<KeyT, ValueT> Results;
  std::mutex Mutex;
};

template <> struct llvm::DenseMapInfo<std::string, void> {
  static inline std::string getEmptyKey() {
    return std::string();
  }

  static inline std::string getTombstoneKey() {
    return std::string("\0");
  }

  static bool isEqual(const std::string &LHS, const std::string &RHS) {
    if (RHS.data() == getEmptyKey().data())
      return LHS.data() == getEmptyKey().data();
    if (RHS.data() == getTombstoneKey().data())
      return LHS.data() == getTombstoneKey().data();
    return LHS == RHS;
  }

  static unsigned getHashValue(const std::string &Val) {
    assert(Val.data() != getEmptyKey().data() &&
      "Cannot hash the empty key!");
    assert(Val.data() != getTombstoneKey().data() &&
      "Cannot hash the tombstone key!");
    return (unsigned)(llvm::hash_value(Val));
  }
};

class FindIncludesAction : public clang::PreprocessorFrontendAction {
public:
  FindIncludesAction(HeaderResults& includeList);
protected:
  void ExecuteAction() override;

public:
  HeaderResults &headerIncludes;
};

class FindIncludesFrontendActionFactory : public clang::tooling::FrontendActionFactory {
public:
  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<FindIncludesAction>(headerIncludes);
  }

  HeaderResults headerIncludes;
};

class BufferedDiagnostics;
class WrapperFactory;
class OutputCapturingFrontendAction;

class ClangToolRunner {
public:
  ClangToolRunner();
  ~ClangToolRunner();
  llvm::Error runTool(clang::tooling::CompilationDatabase &CompDb, clang::tooling::FrontendActionFactory &ActionFactory, 
                      std::vector<std::string> Files, int threadCount);

  void Log(llvm::Twine Msg);
  void AppendError(llvm::Twine Msg);
  std::mutex &getMutex();

  
  void setStopOnFirstError(bool shouldStop) { StopOnFirstError = shouldStop; }
  void setPrintProgress(bool shouldPrint) { PrintProgress = shouldPrint; }
  bool hasErrors();

protected:
  virtual void processFile(const std::string &Path);
  void logDiagnostics(BufferedDiagnostics &buffer);
  friend class OutputCapturingFrontendAction;
  friend class BufferedDiagnostics;

private:
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOptions;
  clang::tooling::CompilationDatabase *Compilations;
  clang::tooling::FrontendActionFactory *Factory;
  bool StopOnFirstError;
  bool PrintProgress;
  int ThreadCount;
  int TotalFiles;
  std::atomic<int> ItemsProcessed;
  std::string ErrorMsg;
  std::mutex TUMutex;
};

class BufferedDiagnostics : public clang::DiagnosticConsumer {
  std::vector<clang::StoredDiagnostic> Out;
  clang::LangOptions LangOpts;
  ClangToolRunner *Owner;

public:
  BufferedDiagnostics(ClangToolRunner *Owner) : Owner(Owner) {
  }
  BufferedDiagnostics() {
  }

  void BeginSourceFile(const clang::LangOptions &LangOpts,
                       const clang::Preprocessor *) override;

  void EndSourceFile() override;

  /// Callback to inform the diagnostic client that processing of all
  /// source files has ended.
  virtual void finish() override {
    
  }

  using diag_iterator = std::vector<clang::StoredDiagnostic>::const_iterator;

  diag_iterator begin() const { return Out.begin(); }
  diag_iterator end() const { return Out.end(); }

  void HandleDiagnostic(clang::DiagnosticsEngine::Level DiagLevel,
    const clang::Diagnostic &Info) override;
  void PrintDiagnostic(clang::DiagnosticOptions &Options);
  const clang::Preprocessor * PP;
};
