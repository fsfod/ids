//===- ExportOptionsConfig.h - Config file system for export macros*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// 
//
//===----------------------------------------------------------------------===//

#ifndef EXPORT_OPTIONS_CONFIG_H
#define EXPORT_OPTIONS_CONFIG_H

#include "clang/Format/Format.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Error.h"
#include <string>
#include <vector>

class ExportOptions;

#define OVERRIDABLE_OPTION_FLAGS(_)                                            \
  _(Disabled, disabled)                                                        \
  _(ExportExternC, exportExternC)                                              \
  _(ExportSimpleClasses, exportSimpleClasses)                                  \
  _(IsGeneratingMacro, isGeneratingMacro)                                      \
  _(ExportMacroHeader, exportMacroHeader)                                      \
  _(ExportMembers, exportMembers)                                              \
  _(AddExportHeaderInclude, addExportHeaderInclude)                            \
  _(ForceExportClassData, forceExportClassData)                                \
  _(ExportGlobalVariableDefs, exportGlobalVariableDefs) 

#define OVERRIDABLE_OPTION_MACROS(_)                                           \
  _(IsGeneratingMacro, isGeneratingMacro)                                      \
  _(ExportMacro, exportMacro)                                                  \
  _(ClassMacro, classMacro)                                                    \
  _(DataMacro, dataMacro)                                                      \
  _(ClassDataMacro, classDataMacro)                                            \
  _(ExternTemplateMacro, externTemplateMacro)                                  \
  _(ExportTemplateMacro, exportTemplateMacro)                                  \
  _(ExternCMacro, externCMacro)                                                \
  _(ExportMacroHeader, exportMacroHeader)

#define OVERRIDABLE_OPTIONS_LIST(_)                                            \
  OVERRIDABLE_OPTION_MACROS(_)                                                 \
  OVERRIDABLE_OPTION_FLAGS(_)                                                  \
  _(PathRoot, pathRoot) \

#define EXPORT_OPTION_LIST(_)                                                  \
  _(HeaderFiles, headerFiles)                                                  \
  _(IgnoredFiles, ignoredFiles)                                                \
  _(ExcludedDirectories, excludedDirectories)                                  \
  _(OtherExportMacros, otherExportMacros)                                      \
  OVERRIDABLE_OPTIONS_LIST(_)

// Make optional<bool> less error prone by making its behavior as a boolean value in conditions
// use the value() instead of has_value()
class BoolOption : public std::optional<bool> {
public:
  constexpr explicit operator bool() const noexcept {
    return has_value() && value();
  }

  BoolOption &operator=(const bool &value) {
    emplace(value);
    return *this;
  }
};

class HeaderPathMatcher;

struct ExportGroup {
  ExportOptions *Owner;
  int Id;
  // optional Name of the group, more than one group can have the same name
  std::string Name;

  // optional if a global export macro is defined
  std::string ExportMacro;
  // optional
  std::vector<std::string> HeaderFiles;
  // HeaderDirectories or HeaderFiles is required
  std::vector<std::string> HeaderDirectories;
  std::vector<std::string> SourceDirectories;
  // If set appended to start of paths in HeaderFiles
  std::string PathRoot;
  // The header that must be included for the export macro declarations
  // an include of this will be added to any files that get export annotations added
  std::string ExportMacroHeader;
  std::string ClassMacro;          
  // optional
  std::string ExternTemplateMacro;
  // optional
  std::string ExternCMacro;
  // optional
  std::string ExportTemplateMacro; 
  // optional used on global variables and optionally on static class variables if
  // ClassDataMacro is not set
  std::string DataMacro;
  // optional used on static class variables
  std::string ClassDataMacro;
  // optional, This is defined while parsing headers scan for targets for
  // export macros. This should cause the code to not define the export macros
  // so the tool can define them to a clang annotate attribute
  std::string IsGeneratingMacro;
  std::vector<std::string> IgnoredFiles;
  std::vector<std::string> ExcludedDirectories;
  std::vector<std::string> IgnoredClasses;
  std::vector<std::string> OtherExportMacros;
  BoolOption ExportExternC;
  BoolOption ExportSimpleClasses;
  BoolOption Disabled;
  BoolOption IsRoot;
  BoolOption ExportTemplates;
  BoolOption ExportMembers;
  BoolOption AddExportHeaderInclude;
  // Export individuals class static variables even if class is already exported
  BoolOption ForceExportClassData;
  BoolOption ExportGlobalVariableDefs;

  // List of header and source file found on disk matched from the pattens this group declares
  std::vector<std::string> HeaderPaths;
  // List of headers explicitly specified by name these will claim ownership of the files to this group over another
  // group matching them by found by directory path wildcard
  std::vector<std::string> ExplicitHeaderPaths;
  std::vector<std::string> SourcePaths;

  ExportGroup()
    : Id(-1) , Owner(nullptr) {
  }

  std::string getName();
  bool isRootGroup() {
    return reinterpret_cast<ExportGroup *>(Owner) == this;
  }

  llvm::Error gatherFiles(bool ignoreMissing);
  llvm::Error createPathFilter(HeaderPathMatcher &filter);
  llvm::Error gatherDirectoryFiles(const std::vector<std::string> &directoryList, std::vector<std::string> &files, bool soureceFiles);
  void dump();

private:
  llvm::StringRef createFullPath(llvm::StringRef path, llvm::SmallString<256>& pathBuff);
};

struct FileOptionEntry {
  std::string Path;
  ExportGroup *Group;
  bool IsHeader;

  FileOptionEntry(llvm::StringRef Path, ExportGroup *Group, bool IsHeader)
    : Path(Path), Group(Group), IsHeader(IsHeader) {
  }
};

class ExportOptions;

class FileOptionLookup {
public:
  void addFile(llvm::StringRef file, ExportGroup *options, bool isheader);  
  ExportGroup* getFileOptions(clang::FileEntryRef file);
  ExportGroup* getFileOptions(llvm::StringRef path);
  bool contains(llvm::StringRef path) { return getFileOptions(path) != nullptr; }

  void getFilesForGroup(ExportGroup *Group, std::vector<llvm::StringRef> &files);

private:
  FileOptionEntry& insertOrUpdateEntry(llvm::StringRef path, ExportGroup *Group, bool isHeader);

  llvm::StringMap<int> Lookup;
  std::vector<FileOptionEntry> Files;
  ExportOptions* options;
};

struct RootExportOptions : ExportGroup {
  std::string ClangFormatFile;
  std::vector<std::unique_ptr<ExportGroup>> Groups;
};

class ExportOptions : public RootExportOptions {
public:
  ExportOptions();
  ~ExportOptions();

  static llvm::Error loadFromFile(llvm::StringRef path, ExportOptions& options);
  static llvm::Error loadFromDirectory(const std::string &path, ExportOptions& options);
  static bool directoryHasExportConfig(const std::string &path);

  void setOverridesAndDefaults(const ExportGroup &options);
  llvm::Error ScanForFiles();
  llvm::Error gatherAllFiles(std::vector<std::string> &allFiles, FileOptionLookup &fileOptions);
  void dump();
  std::vector<std::unique_ptr<ExportGroup>>& getGroups() { return Groups; }

  ExportGroup *getGroup(llvm::StringRef name) {
    for (auto& group : Groups) {
      if (name.compare_insensitive(group->Name) == 0)
        return group.get();
    }
    return nullptr;
  }

  ExportGroup *getGroup(int Id) {
    if (Id == 0) {
      return this;
    } else {
      return Groups[Id-1].get();
    }
  }

  clang::format::FormatStyle* getClangFormatStyle();

  const std::string &getRootDirectory() { return RootDirectory; }
  llvm::Error setRootDirectory(llvm::StringRef RootDirectory);
  llvm::Error tryLoadClangFormatFile();

private:
  llvm::Error Load(llvm::StringRef text);
  std::string RootDirectory;
  bool ClangFormatValid;
  clang::format::FormatStyle ClangFormatStyle;
};

#endif
