#include "ExportOptionsConfig.h"
#include "FindIncludes.h"
#include "clang/Basic/FileManager.h"
#include "clang/Format/Format.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"

using namespace llvm::json;
using namespace llvm;

static bool fromJSON(const json::Value &E, ExportGroup &opts, json::Path P);


ExportOptions::ExportOptions() 
 : ClangFormatValid(false) {
}
ExportOptions::~ExportOptions() = default;

llvm::Error ExportOptions::loadFromFile(StringRef path, ExportOptions& options) {

  ErrorOr<std::unique_ptr<MemoryBuffer>> bufferOrErr = MemoryBuffer::getFile(path);

  if (std::error_code ec = bufferOrErr.getError())
    return make_error<StringError>("Error loading export options file:", ec);

  Error err = options.setRootDirectory(llvm::sys::path::parent_path(path));
  if (err) {
    return err;
  }
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

bool ExportOptions::directoryHasExportConfig(const std::string &path) {
  SmallString<256> pathBuf(path);
  sys::path::append(pathBuf, "export_options.json");

  return sys::fs::is_regular_file(pathBuf);
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

  Owner = this;
  if (!fromJSON(*Val, *this, RootPath)) 
    return RootPath.getError();

  std::vector<ExportGroup> GroupList;

  if (!(Mapper.mapOptional("groups", GroupList) &&
    Mapper.mapOptional("clangFormatFile", ClangFormatFile)))
    return RootPath.getError();

  // Just set this so code can be simpler and not worry if there exports options is from a group or not
  Owner = this;
  Id = 0;
  for (size_t i = 0; i < GroupList.size(); i++) {
    ExportGroup &group = GroupList[i];
    group.Owner = this;
    group.Id = i + 1;
    Groups.emplace_back(std::make_unique<ExportGroup>(std::move(group)));
  } 

  if (auto err = tryLoadClangFormatFile()) {
    return err;
  }

  return Error::success();
}

Error ExportOptions::setRootDirectory(llvm::StringRef RootDirectory) {
  SmallString<255> buffer = RootDirectory;

  std::error_code ec = llvm::sys::fs::real_path(RootDirectory, buffer);
  if (ec) {
    return createStringError(ec, "Failed to resolve full path for export config root directory\n");
  }
  this->RootDirectory = buffer.str();

  return tryLoadClangFormatFile();
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
  setDefault(group->fieldName, fieldName);

#define SET_OPTION_OVERRIDE(fieldName, optName) \
  setOverride(fieldName, options.fieldName);

void ExportOptions::setOverridesAndDefaults(const ExportGroup &options) {

  OVERRIDABLE_OPTIONS_LIST(SET_OPTION_OVERRIDE);

  if (!Disabled.has_value()) {
    Disabled = false;
  }
 
  OtherExportMacros.insert(OtherExportMacros.end(), options.OtherExportMacros.begin(), options.OtherExportMacros.end());

  for (auto &group : Groups) {
    OVERRIDABLE_OPTIONS_LIST(SET_OPTION_DEFAULT);
    group->OtherExportMacros.insert(group->OtherExportMacros.end(), OtherExportMacros.begin(), OtherExportMacros.end());
  }
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

bool fromJSON(const json::Value &E, ExportGroup &opts, json::Path P) {
  ObjectMapper map(E, P);

  if (!opts.isRootGroup()) {
    if (!map.mapOptional("name", opts.Name))
      return false;

    if (!map.mapOptional("headerDirectories", opts.HeaderDirectories))
      return false;

    if (!map.mapOptional("sourceDirectories", opts.SourceDirectories))
      return false;
  }

  bool valid = READ_ALL_OPTIONAL_VALUES();
  if (!valid)
    return false;

  // Default to adding include of export header if specified, child groups can then disable it if needed
  if (!opts.ExportMacroHeader.empty() && !opts.AddExportHeaderInclude.has_value()) {
    opts.AddExportHeaderInclude = true;
  }

  if (!opts.isRootGroup() && opts.HeaderFiles.empty() && opts.HeaderDirectories.empty() && opts.SourceDirectories.empty()) {
    P.report("headerFiles or headerDirectories field must be defined and non empty");
    return false;
  }

  return true;
}

Error ExportGroup::createPathFilter(HeaderPathMatcher &filter) {
  if (!IgnoredFiles.empty()) {
    if (auto Err = filter.addPaths(IgnoredFiles, PathMatchMode::End))
      return Err;
  }

  Error err = filter.addDirectoryRoots(ExcludedDirectories);
  if (err) {
    return err;
  }

  return Error::success();
}

llvm::Error ExportGroup::gatherDirectoryFiles(const std::vector<std::string> &directoryList, std::vector<std::string> &files, bool sourceFiles) {
  auto &DirList = sourceFiles ? SourceDirectories : HeaderDirectories;
  HeaderPathMatcher filter;

  if (Error err = createPathFilter(filter)) {
    return err;
  }

  bool SkipRootFiles = false;

  auto FilterFunc = [&](StringRef path) {
    if (!sourceFiles && !isHeaderFile(path)) {
      llvm::outs() << "Skipped non header file: " << path << "\n";
      return true;
    } else if(sourceFiles && sys::path::extension(path) != ".cpp") {
      llvm::outs() << "Skipped non source file: " << path << "\n";
      return true;
    }
    // Skip files in root directory and require them to explicitly be added
    if (SkipRootFiles && !sys::path::has_parent_path(path))
      return true;
    return filter.match(path);
  };

  llvm::SmallString<256> headerDirectory;
  // check if you user wanted us to recursively find all headers in our root folder or PathRoot
  if (directoryList.size() == 1 && directoryList[0] == "*") {
    createFullPath("", headerDirectory);
    SkipRootFiles = true;
    if (auto err = GatherFilesInDirectory(headerDirectory, files, FilterFunc))
      return err;
  } else {
    for (auto& dirPath : directoryList) {
      createFullPath(dirPath, headerDirectory);

      if (auto err = GatherFilesInDirectory(headerDirectory, files, FilterFunc)) {
        return err;
      }
    }
  }

  return Error::success();
}

StringRef ExportGroup::createFullPath(StringRef path, SmallString<256> &pathBuff) {
  pathBuff.clear();

  StringRef rootDirectory = Owner->getRootDirectory();

  if (!PathRoot.empty()) {
    sys::path::append(pathBuff, rootDirectory, PathRoot, path);
  } else {
    sys::path::append(pathBuff, rootDirectory, path);
  }
  sys::path::remove_dots(pathBuff, true);

  // Canonicalize native path to avoid mixed separator styles.
  sys::path::native(pathBuff);

  size_t EndPathLength = path.size();
  size_t PathStart = pathBuff.size() - EndPathLength;
  if (pathBuff[PathStart] == '\\' || pathBuff[PathStart] == '/')
    PathStart++;

  return pathBuff.substr(PathStart, EndPathLength);
}

llvm::Error ExportGroup::gatherFiles(bool ignoreMissing) {
  SmallString<256> pathBuff;
  StringSet SeenHeaders;
  bool hasMissing = false;
  for (auto& path : HeaderFiles) {
    createFullPath(path, pathBuff);

    if (!sys::fs::exists(pathBuff)) {
      hasMissing = true;
      if (ignoreMissing) {
      } else {
        llvm::errs() << "File '" << path << "' in specified in headerFiles field does not exist.\n";
      }
      continue;
    }

    sys::fs::file_status fileStat;
    std::error_code ec = llvm::sys::fs::status(pathBuff, fileStat);
    if (ec) {
      return createStringError(ec, "Error while accessing file '" + path + "' listed in export_options.json headerFiles.\n");
    }
    SeenHeaders.insert(pathBuff);
    ExplicitHeaderPaths.push_back(pathBuff.str().str());
  }

  if (hasMissing && !ignoreMissing)
    return createStringError("Header files in export group "  + getName() + " are missing");

  std::vector<std::string> files;
  if (auto err = gatherDirectoryFiles(HeaderDirectories, files, false)) {
    return err;
  }

  // Don't add duplicate entries for files
  for (auto& path : files) {
    if (!SeenHeaders.contains(path)) {
      HeaderPaths.push_back(std::move(path));
    }
  }

  if (SourceDirectories.empty())
    return Error::success();

  return gatherDirectoryFiles(SourceDirectories, SourcePaths, true);
}

std::string ExportGroup::getName() {
  if (isRootGroup()) {
    return "Root";
  } else if (!Name.empty()) {
    return Name;
  } else {
    return std::to_string(Id);
  }
}

static Error getFileRealPath(StringRef path, SmallString<255>& pathBuffer) {
  pathBuffer.clear();
  std::error_code EC = llvm::sys::fs::real_path(path, pathBuffer, true);
  if (EC) {
    return make_error<StringError>("Error getting real path for '" + path + "': ", EC);
  }

  return Error::success();
}

Error ExportOptions::scanForFiles() {
  for (auto &group : Groups) {
    if (group->Disabled)
      continue;

    Error err = group->gatherFiles(false);
    if (err)
      return err;
  }

  return Error::success();
}

Error ExportOptions::gatherAllFiles(std::vector<std::string> &allFiles, FileOptionLookup& fileOptions) {

  for (auto &group : Groups) {
    if (group->Disabled)
      continue;

    for (auto &path : group->SourcePaths) {
      allFiles.push_back(path);
      fileOptions.addFile(path, group.get(), false);
    }

    for (auto& path : group->HeaderPaths) {
      fileOptions.addFile(path, group.get(), true);
      allFiles.push_back(path);
    }
  }

  // Explicitly specified header files options should override any options from a HeaderDirectory group
  for (auto &group : Groups) {
    if (group->Disabled)
      continue;

    for (auto& path : group->ExplicitHeaderPaths) {
      if (!fileOptions.contains(path)) {
        allFiles.push_back(path);
      }
      fileOptions.addFile(path, group.get(), true);
    }
  }
  
  return Error::success();
}


static void printField(StringRef fieldName, std::optional<bool>& field) {
  if (field.has_value()) {
    llvm::outs() << "  " << fieldName << ": " << (field.value() ? "true" : "false") << "\n";
  }
}

static void printField(StringRef fieldName, StringRef field) {
  if (!field.empty()) {
    llvm::outs() << "  " << fieldName << ": " << field << "\n";
  }
}

static void printField(StringRef fieldName, std::vector<std::string>& field) {
  if (!field.empty()) {
    llvm::outs() << "  " << fieldName << ":\n";
    for (auto& item : field) {
      llvm::outs() << "    " << item << "\n";
    }
  }
}

#define PRINT_FIELD(field, fieldName) printField(#fieldName, field);

void ExportGroup::dump() {
  OVERRIDABLE_OPTIONS_LIST(PRINT_FIELD);
  printField("HeaderPaths", HeaderPaths);
  printField("ExplicitHeaderPaths", ExplicitHeaderPaths);
  printField("SourcePaths", SourcePaths);
}

void FileOptionLookup::addFile(llvm::StringRef file, ExportGroup *options, bool isheader) {
  insertOrUpdateEntry(file, options, isheader);

  SmallString<255> pathBuffer;
  llvm::sys::fs::real_path(file, pathBuffer, true);
  // If the path has different case or has symbolic links in it, add the fully resolved path as well to the lookup.
  if (pathBuffer.str() != file) {
    insertOrUpdateEntry(pathBuffer, options, isheader);
  }
}

FileOptionEntry& FileOptionLookup::insertOrUpdateEntry(llvm::StringRef path, ExportGroup *Group, bool isHeader) {
  auto Slot = Lookup.find(path);

  if (Slot == Lookup.end()) {
    Files.push_back(FileOptionEntry(path, Group, isHeader));
    Lookup[path.str()] = Files.size() - 1;
    return Files.back();
  } else {
    Files[Slot->second].Group = Group;
    return Files[Slot->second];
  }
}

ExportGroup *FileOptionLookup::getFileOptions(clang::FileEntryRef file) {
  StringRef RealName = file.getFileEntry().tryGetRealPathName();
  if (RealName == "")
    return nullptr;
  return getFileOptions(RealName);
}

ExportGroup *FileOptionLookup::getFileOptions(llvm::StringRef path) {

  auto it = Lookup.find(path);
  if (it != Lookup.end()) {
    return Files[it->second].Group;
  }

  SmallString<255> buffer;
  std::error_code EC = llvm::sys::fs::real_path(path, buffer, true);
  if (EC)
    return nullptr;

  it = Lookup.find(buffer);
  if (it != Lookup.end()) {
    return Files[it->second].Group;
  }

  return nullptr;
}

void FileOptionLookup::getFilesForGroup(ExportGroup *Group, std::vector<llvm::StringRef> &FileList) {
  for (auto& file : Files) {
    if (file.Group == Group) {
      FileList.push_back(file.Path);
    }
  }
}
