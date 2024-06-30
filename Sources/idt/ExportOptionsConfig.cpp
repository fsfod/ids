#include "llvm/Support/JSON.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Path.h"
#include "ExportOptionsConfig.h"

using namespace llvm::json;
using namespace llvm;

static bool fromJSON(const json::Value &E, BaseExportOptions &opts, json::Path P);

llvm::Error ExportOptions::LoadFromFile(StringRef path, ExportOptions& options) {

  ErrorOr<std::unique_ptr<MemoryBuffer>> bufferOrErr = MemoryBuffer::getFile(path);

  if (std::error_code ec = bufferOrErr.getError())
    return make_error<StringError>("Error loading export options file:", ec);

  return options.Load(bufferOrErr.get()->getBuffer());
}

Error ExportOptions::LoadFromDirectory(const std::string &path, ExportOptions& options) {
  SmallString<256> pathBuf(path);
  sys::path::append(pathBuf, "export_options.json");
  if (!sys::fs::is_regular_file(pathBuf)) {
    return createStringError(errc::no_such_file_or_directory, "directory has no export_options.json");
  }
  return LoadFromFile(pathBuf, options);
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

void ExportOptions::PropagateDefaults(const std::string& exportMacro) {

  if (!exportMacro.empty()) {
    ExportMacro = exportMacro;
  }

  if (ClassMacro.empty()) {
    ClassMacro = ExportMacro;
  }

  if (ExternCMacro.empty()) {
    ExternCMacro = ExportMacro;
  }

  for (auto &group : Groups) {
    if (group.ExportMacro.empty()) {
      group.ExportMacro = ExportMacro;
    }

    if (group.ClassMacro.empty()) {
      group.ClassMacro = group.ExportMacro;
    }

    if (group.ExternCMacro.empty()) {
      group.ExternCMacro = group.ExportMacro;
    }
  }
}

bool fromJSON(const json::Value &E, HeaderGroupOptions &opts, json::Path P) {
  ObjectMapper map(E, P);

  if (!map.mapOptional("name", opts.Name))
    return false;

  if (!map.mapOptional("headerDirectory", opts.HeaderDir))
    return false;

  if (!map.mapOptional("headers", opts.Headers))
    return false;

  if (opts.Headers.empty() && opts.HeaderDir.empty()) {
    P.report("headers or headerDirectory field must be defined and non empty");
    return false;
  }

  return fromJSON(E, static_cast<BaseExportOptions&>(opts), P);
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

  if (opts.IsRoot && !map.mapOptional("headers", opts.Headers))
    return false;

  if(!map.mapOptional("exportSimpleClasses", opts.ExportSimpleClasses))
    return false;

  if(!map.mapOptional("exportExternC", opts.ExportExternC))
    return false;

  if (!(map.mapOptional("exportMacro", opts.ExportMacro) &&
        map.mapOptional("classMacro", opts.ClassMacro) &&
        map.mapOptional("externTemplateMacro", opts.ExternTemplateMacro) &&
        map.mapOptional("exportTemplateMacro", opts.ExportTemplateMacro) &&
        map.mapOptional("externCMacro", opts.ExternCMacro) &&
        map.mapOptional("ignoredHeaders", opts.IgnoredHeaders)))
    return false;
  
  return true;
}