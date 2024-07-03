#include "clang/Basic/FileManager.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Path.h"
#include "ExportOptionsConfig.h"
#include "FindIncludes.h"

using namespace llvm::json;
using namespace llvm;

static bool fromJSON(const json::Value &E, BaseExportOptions &opts, json::Path P);

llvm::Error ExportOptions::loadFromFile(StringRef path, ExportOptions& options) {

  ErrorOr<std::unique_ptr<MemoryBuffer>> bufferOrErr = MemoryBuffer::getFile(path);

  if (std::error_code ec = bufferOrErr.getError())
    return make_error<StringError>("Error loading export options file:", ec);

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

  if (!Mapper.mapOptional("groups", Groups))
    return RootPath.getError();

  return Error::success();
}

void setOverride(std::string& value, const std::string& override) {
  if (!override.empty())
    value = override;
}

void setDefault(std::string& value, const std::string &defaultValue) {
  if (value.empty())
    value = defaultValue;
}

void ExportOptions::setOverridesAndDefaults(const BaseExportOptions &options) {

  setOverride(ExportMacro, options.ExportMacro);
  setOverride(ClassMacro, options.ClassMacro);
  setOverride(ExternCMacro, options.ExternCMacro);
  setOverride(ExportTemplateMacro, options.ExportTemplateMacro);
  setOverride(ExternTemplateMacro, options.ExternTemplateMacro);
  setOverride(ExportMacro, options.ExportMacro);
  setOverride(IsGeneratingMacro, options.IsGeneratingMacro);
 
  OtherExportMacros.insert(OtherExportMacros.end(), options.OtherExportMacros.begin(), options.OtherExportMacros.end());

  if (options.ExportExternC)
    ExportExternC = true;

  if (options.ExportSimpleClasses)
    ExportSimpleClasses = true;

  for (auto &group : Groups) {
    setDefault(group.ExportMacro, ExportMacro);
    setDefault(group.ClassMacro, ClassMacro);
    setDefault(group.ExportTemplateMacro, ExportTemplateMacro);
    setDefault(group.ExternTemplateMacro, ExternTemplateMacro);
    setDefault(group.ExternCMacro, ExternCMacro);
    setDefault(group.IsGeneratingMacro, IsGeneratingMacro);

    group.OtherExportMacros.insert(group.OtherExportMacros.end(), OtherExportMacros.begin(), OtherExportMacros.end());
  }
}

bool fromJSON(const json::Value &E, HeaderGroupOptions &opts, json::Path P) {
  ObjectMapper map(E, P);

  if (!map.mapOptional("name", opts.Name))
    return false;

  if (!map.mapOptional("headerDirectories", opts.HeaderDirectories))
    return false;

  if(!fromJSON(E, static_cast<BaseExportOptions&>(opts), P))
    return false;

  if (opts.HeaderFiles.empty() && opts.HeaderDirectories.empty()) {
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

bool fromJSON(const json::Value &E, BaseExportOptions &opts, json::Path P) {
  ObjectMapper map(E, P);

  if(!map.mapOptional("exportSimpleClasses", opts.ExportSimpleClasses))
    return false;

  if(!map.mapOptional("exportExternC", opts.ExportExternC))
    return false;

  if (!(map.mapOptional("headerFiles", opts.HeaderFiles) && 
        map.mapOptional("exportMacro", opts.ExportMacro) &&
        map.mapOptional("classMacro", opts.ClassMacro) &&
        map.mapOptional("externTemplateMacro", opts.ExternTemplateMacro) &&
        map.mapOptional("exportTemplateMacro", opts.ExportTemplateMacro) &&
        map.mapOptional("externCMacro", opts.ExternCMacro) &&
        map.mapOptional("ignoredHeaders", opts.IgnoredHeaders) &&
        map.mapOptional("isGeneratingMacro", opts.IsGeneratingMacro) && 
        map.mapOptional("otherExportMacros", opts.OtherExportMacros)))
    return false;
  
  return true;
}

Error HeaderGroupOptions::gatherDirectoryFiles(llvm::StringRef rootDirectory, std::vector<std::string> &files) {
  if (HeaderDirectories.empty())
    return Error::success();

  HeaderPathMatcher filter;

  if (!IgnoredHeaders.empty()) {
    auto filterOrErr = HeaderPathMatcher::create(IgnoredHeaders);
    if (!filterOrErr) {
      return filterOrErr.takeError();
    }
    filter = std::move(filterOrErr.get());
  }

  llvm::SmallString<256> headerDirectory;
  for (auto& dirPath : HeaderDirectories) {
    llvm::sys::path::append(headerDirectory, rootDirectory, dirPath);
    std::replace(headerDirectory.begin(), headerDirectory.end(), '\\', '/');

    if (auto err = GatherFilesInDirectory(headerDirectory, files, &filter)) {
      return err;
    }
  }
  return Error::success();
}

Error BaseExportOptions::gatherFiles(llvm::StringRef rootDirectory, std::vector<std::string> &files) {
  SmallString<256> pathBuff;
  for (auto& path : HeaderFiles) {
    sys::path::append(pathBuff, rootDirectory, path);
    std::replace(pathBuff.begin(), pathBuff.end(), '\\', '/');

    if (!sys::fs::exists(pathBuff)) {
      llvm::errs() << "File '" << path << "' in export_options.json headerFiles field does not exist.\n";
      continue;
    }

    sys::fs::file_status fileStat;
    if (std::error_code ec = llvm::sys::fs::status(path, fileStat)) {
      return createStringError(ec, "Error while accessing file '" + path + "' listed in export_options.json headerFiles.\n");
    }
    
    files.push_back(pathBuff.str().str());
  }

  return Error::success();
}

Error ExportOptions::gatherAllFiles(llvm::StringRef rootDirectory, std::vector<std::string> &allFiles, FileOptionLookup& fileOptions) {
  clang::FileManager *FileMgr =
    new clang::FileManager(clang::FileSystemOptions(), llvm::vfs::getRealFileSystem());

  llvm::SmallString<256> headerDirectory;
  std::vector<std::string> files;

  for (HeaderGroupOptions &group : Groups) {
    files.clear();
    if (auto err = group.gatherDirectoryFiles(rootDirectory, files)) {
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

  // Explicitly specified header files options should override any options from a HeaderDirectory group
  for (HeaderGroupOptions &group : Groups) {
    files.clear();
    group.gatherFiles(rootDirectory, files);
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