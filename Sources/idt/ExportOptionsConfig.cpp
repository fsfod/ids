#include "ExportOptionsConfig.h"
#include "FindIncludes.h"
#include "clang/Basic/FileManager.h"
#include "clang/Format/Format.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"

using namespace llvm::json;
using namespace llvm;

static bool fromJSON(const json::Value &E, BaseExportOptions &opts, json::Path P);


ExportOptions::ExportOptions() 
 : ClangFormatValid(false) {
}
ExportOptions::~ExportOptions() = default;

llvm::Error ExportOptions::loadFromFile(StringRef path, ExportOptions& options) {

  ErrorOr<std::unique_ptr<MemoryBuffer>> bufferOrErr = MemoryBuffer::getFile(path);

  if (std::error_code ec = bufferOrErr.getError())
    return make_error<StringError>("Error loading export options file:", ec);

  options.RootDirectory = llvm::sys::path::parent_path(path).str();
  return options.Load(bufferOrErr.get()->getBuffer());
}

Error ExportOptions::loadFromDirectory(const std::string &path, ExportOptions& options) {
  SmallString<256> pathBuf(path);
  sys::path::append(pathBuf, "export_options.json");

  if (!sys::fs::is_regular_file(pathBuf)) {
    return createStringError(errc::no_such_file_or_directory, "directory has no export_options.json");
  }
  return loadFromFile(pathBuf, options);
}

clang::format::FormatStyle* ExportOptions::getClangFormatStyle() {
 return ClangFormatValid ? &ClangFormatStyle : NULL;
}

std::error_code loadAndParseConfigFile(StringRef ConfigFile, clang::format::FormatStyle *Style) {
  auto Text =
    llvm::vfs::getRealFileSystem().get()->getBufferForFile(ConfigFile.str());
  if (auto EC = Text.getError())
    return EC;
  if (auto EC = clang::format::parseConfiguration(*Text.get(), Style)) {
    return EC;
  }
  return std::error_code();
}

Error ExportOptions::Load(StringRef text) {
  auto Val = parse(text);

  if (!Val)
    return Val.takeError();

  auto *Root = Val->getAsObject();
  if (!Root)
    return make_error<StringError>("not a JSON object", inconvertibleErrorCode());

  auto VersionStr = Root->getString("version");
  if (!VersionStr)
    return createStringError("required field 'version' not specified");

  int Version;
  if (VersionStr->getAsInteger(10, Version))
    return createStringError("invalid version number");

  if (Version < 1 || Version > 1)
    return createStringError("unsupported version");

  Path::Root RootPath("ExportOptions");
  ObjectMapper Mapper(*Val, RootPath);

  if (!fromJSON(*Val, *this, RootPath)) 
    return RootPath.getError();

  if (!(Mapper.mapOptional("groups", Groups) &&
        Mapper.mapOptional("clangFormatFile", ClangFormatFile)))
    return RootPath.getError();

  // Just set this so code can simpler and not worry if there exports options is from a group or not
  Owner = this;
  for (auto& group : Groups) {
    group.Owner = this;
  }

  if (auto err = tryLoadClangFormatFile()) {
    return err;
  }

  return Error::success();
}

Error ExportOptions::tryLoadClangFormatFile() {
  if (ClangFormatFile.empty() || ClangFormatValid || RootDirectory.empty())
    return Error::success();

  SmallString<256> path(ClangFormatFile);
  sys::fs::make_absolute(RootDirectory, path);
  ClangFormatStyle = clang::format::getNoStyle();

  if (auto err = loadAndParseConfigFile(path, &ClangFormatStyle))
    return createStringError(err, "bad .clang_format file specified in export_options.json");

  ClangFormatValid = true;
  return Error::success();
}

void setOverride(std::string& value, const std::string& override) {
  if (!override.empty())
    value = override;
}

void setOverride(std::optional<bool> &value, const std::optional<bool> & override) {
  if (override.has_value())
    value = override;
}

void setDefault(std::string& value, const std::string &defaultValue) {
  if (value.empty())
    value = defaultValue;
}

void setDefault(std::optional<bool> &value, const std::optional<bool> &defaultValue) {
  if (!value.has_value() && defaultValue.has_value()) {
    value = defaultValue;
  } else if (!value.has_value()) {
    value = false;
  }
}

#define SET_OPTION_DEFAULT(fieldName, optName) \
  setDefault(group.fieldName, fieldName);

#define SET_OPTION_OVERRIDE(fieldName, optName) \
  setOverride(fieldName, options.fieldName);

void ExportOptions::setOverridesAndDefaults(const BaseExportOptions &options) {

  OVERRIDABLE_OPTIONS_LIST(SET_OPTION_OVERRIDE);

  if (!Disabled.has_value()) {
    Disabled = false;
  }
 
  OtherExportMacros.insert(OtherExportMacros.end(), options.OtherExportMacros.begin(), options.OtherExportMacros.end());

  for (auto &group : Groups) {
    OVERRIDABLE_OPTIONS_LIST(SET_OPTION_DEFAULT);
    group.OtherExportMacros.insert(group.OtherExportMacros.end(), OtherExportMacros.begin(), OtherExportMacros.end());
  }
}

bool fromJSON(const json::Value &E, HeaderGroupOptions &opts, json::Path P) {
  ObjectMapper map(E, P);

  if (!map.mapOptional("name", opts.Name))
    return false;

  if (!map.mapOptional("headerDirectories", opts.HeaderDirectories))
    return false;

  if (!map.mapOptional("sourceDirectories", opts.SourceDirectories))
    return false;

  if(!fromJSON(E, static_cast<BaseExportOptions&>(opts), P))
    return false;

  if (opts.HeaderFiles.empty() && opts.HeaderDirectories.empty() && opts.SourceDirectories.empty()) {
    P.report("headerFiles or headerDirectories field must be defined and non empty");
    return false;
  }

  return true;
}

template<typename T> bool MapEnumFlag(ObjectMapper &map, StringLiteral prop, T &enumField, T enumValue) {
  bool value = false;
  if (!map.mapOptional(prop, value))
    return false;
  if (value)
    enumField = static_cast<T>(static_cast<int>(enumField) | static_cast<int>(enumValue));
  return true;
}

#define READ_OPTION_ENTRY(fieldName, optName) \
  map.mapOptional(#optName, opts.fieldName) &&

#define READ_ALL_OPTIONAL_VALUES() (EXPORT_OPTION_LIST(READ_OPTION_ENTRY) true)

bool fromJSON(const json::Value &E, BaseExportOptions &opts, json::Path P) {
  ObjectMapper map(E, P);

  bool valid = READ_ALL_OPTIONAL_VALUES();
  if (!valid)
    return false;

  // Default to adding include of export header if specified, child groups can then disable it if needed
  if (!opts.ExportMacroHeader.empty() && !opts.AddExportHeaderInclude.has_value()) {
    opts.AddExportHeaderInclude = true;
  }
  return true;
}

Error HeaderGroupOptions::gatherDirectoryFiles(std::vector<std::string> &files) {
  if (HeaderDirectories.empty())
    return Error::success();

  HeaderPathMatcher filter;

  if (!IgnoredHeaders.empty()) {
    if (auto Err = filter.addPaths(IgnoredHeaders, PathMatchMode::End)) 
      return Err;
  }

  if (!ExcludedDirectories.empty()) {
    llvm::SmallString<256> Patten;
    for (auto &path : ExcludedDirectories) {
      Patten.clear();
      Patten = path;

      // Make sure the path ends in a path separator so we don't partially match a directory name
      if (Patten.front() == '\\') {
        Patten[Patten.size() - 1] = '/';
      } else if (Patten.front() != '\\') {
        Patten += '/';
      }

      if (auto Err = filter.addPath(Patten, PathMatchMode::Start))
        return Err;
    }
  }

  bool SkipRootFiles = false;
  auto FilterFunc = [&](StringRef path) {
    if (!isHeaderFile(path)) {
      llvm::outs() << "Skipped non header file: " << path << "\n";
      return true;
    }
    // Skip files in root directory and require them to explicitly be added
    if (SkipRootFiles && !path.contains('/'))
      return true;
    return filter.match(path);
  };

  llvm::SmallString<256> headerDirectory;
  // check if you user wanted us to recursively find all headers in our root folder or PathRoot
  if (HeaderDirectories.size() == 1 && HeaderDirectories[0] == "*") {
    createFullPath("", headerDirectory);
    SkipRootFiles = true;
    if (auto err = GatherFilesInDirectory(headerDirectory, files, FilterFunc))
      return err;
  } else {
    for (auto& dirPath : HeaderDirectories) {
      createFullPath(dirPath, headerDirectory);

      if (auto err = GatherFilesInDirectory(headerDirectory, files, FilterFunc)) {
        return err;
      }
    }
  }

  return Error::success();
}

llvm::Error HeaderGroupOptions::gatherSourceFiles(std::vector<std::string> &files) {
  if (SourceDirectories.empty())
    return Error::success();

  HeaderPathMatcher filter;

  if (!ExcludedDirectories.empty()) {
    llvm::SmallString<256> Patten;
    for (auto &path : ExcludedDirectories) {
      Patten.clear();
      Patten = path;

      // Make sure the path ends in a path separator so we don't partially match a directory name
      if (Patten.front() == '\\') {
        Patten[Patten.size() - 1] = '/';
      } else if (Patten.front() != '\\') {
        Patten += '/';
      }

      if (auto Err = filter.addPath(Patten, PathMatchMode::Start))
        return Err;
    }
  }

  llvm::SmallString<256> headerDirectory;
  for (auto& dirPath : SourceDirectories) {
    createFullPath(dirPath, headerDirectory);

    auto FilterFunc = [&](StringRef Path) {
      if (sys::path::extension(Path) != ".cpp") {
        llvm::outs() << "Skipped non source file: " << Path << "\n";
        return true;
      }
      return filter.match(Path);
    };

    if (auto err = GatherFilesInDirectory(headerDirectory, files, FilterFunc)) {
      return err;
    }
  }
  return Error::success();
}

StringRef BaseExportOptions::createFullPath(StringRef path, SmallString<256> &pathBuff) {
  pathBuff.clear();

  StringRef rootDirectory = Owner->getRootDirectory();

  if (!PathRoot.empty()) {
    sys::path::append(pathBuff, rootDirectory, PathRoot, path);
  } else {
    sys::path::append(pathBuff, rootDirectory, path);
  }

  std::replace(pathBuff.begin(), pathBuff.end(), '\\', '/');
  
  size_t EndPathLength = path.size();
  size_t PathStart = pathBuff.size() - EndPathLength;
  if (pathBuff[PathStart] == '//')
    PathStart++;

  return pathBuff.substr(PathStart, EndPathLength);
}

Error BaseExportOptions::gatherFiles(std::vector<std::string> &files) {
  SmallString<256> pathBuff;
  for (auto& path : HeaderFiles) {
    createFullPath(path, pathBuff);

    if (!sys::fs::exists(pathBuff)) {
      llvm::errs() << "File '" << path << "' in export_options.json headerFiles field does not exist.\n";
      continue;
    }

    sys::fs::file_status fileStat;
    if (std::error_code ec = llvm::sys::fs::status(pathBuff, fileStat)) {
      return createStringError(ec, "Error while accessing file '" + path + "' listed in export_options.json headerFiles.\n");
    }
    
    files.push_back(pathBuff.str().str());
  }

  return Error::success();
}

Error ExportOptions::gatherAllFiles(std::vector<std::string> &allFiles, FileOptionLookup& fileOptions) {
  clang::FileManager *FileMgr =
    new clang::FileManager(clang::FileSystemOptions(), llvm::vfs::getRealFileSystem());

  std::vector<std::string> files;

  for (HeaderGroupOptions &group : Groups) {
    if (group.Disabled)
      continue;

    files.clear();
    if (auto err = group.gatherDirectoryFiles(files)) {
      return err;
    }

    for (auto& path : files) {
      auto fileRef = FileMgr->getFileRef(path);
      if (!fileRef) 
        return createStringError("Failed to get FileRef for '" + path + "': " + llvm::toString(fileRef.takeError()));

      fileOptions[fileRef->getUniqueID()] = &group;
      allFiles.push_back(std::move(path));
    }
  }

  StringMap<HeaderGroupOptions*> sourceFiles;
  std::vector<std::string> SourceFileList;

  for (int i = 0; i < Groups.size(); i++) {
    HeaderGroupOptions &group = Groups[i];
    if (group.Disabled)
      continue;

    files.clear();
    if (auto err = group.gatherSourceFiles(files)) {
      return err;
    }

    for (auto &path : files) {
      SourceFileList.push_back(path);
      sourceFiles.insert_or_assign(std::move(path), &group);
    }
  }

  if (auto Err = SoftFilterPotentialExports(SourceFileList))
    return Err;

  for (auto& s : SourceFileList) {
    auto fileRef = FileMgr->getFileRef(s);
    if (!fileRef)
      return createStringError("Failed to get FileRef for '" + s + "': " + llvm::toString(fileRef.takeError()));

    fileOptions[fileRef->getUniqueID()] = sourceFiles[s];
    allFiles.push_back(s);
  }
  // Explicitly specified header files options should override any options from a HeaderDirectory group
  for (HeaderGroupOptions &group : Groups) {
    if (group.Disabled)
      continue;

    files.clear();
    Error err = group.gatherFiles(files);
    if (err)
      return err;

    for (auto& path : files) {
      auto fileRef = FileMgr->getFileRef(path);
      if (!fileRef)
        return createStringError("Failed to get FileRef for '" + path + "': " + llvm::toString(fileRef.takeError()));
      fileOptions[fileRef->getUniqueID()] = &group;
      allFiles.push_back(std::move(path));
    }
  }

  return Error::success();
}
