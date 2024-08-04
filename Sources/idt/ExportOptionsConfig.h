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

#include <clang/Format/Format.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Error.h>
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
  _(ForceExportClassData, forceExportClassData) 

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
  _(IgnoredHeaders, ignoredHeaders)                                            \
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

struct BaseExportOptions {
  ExportOptions *Owner;
  // optional
  std::vector<std::string> HeaderFiles;
  // If set appended to start of paths in HeaderFiles
  std::string PathRoot;
  std::string ExportMacroHeader;
  // optional if a global export macro is defined
  std::string ExportMacro;         
  std::string ClassMacro;          
  // optional
  std::string ExternTemplateMacro;
  // optional
  std::string ExternCMacro;
  // optional
  std::string ExportTemplateMacro; 
  // optional used on global varibles and optionally on static class variables if
  // ClassDataMacro is not set
  std::string DataMacro;
  // optional used on static class variables
  std::string ClassDataMacro;
  // optional, This is defined while parsing headers scan for targets for
  // export macros. This should cause the code to not define the export macros
  // so the tool can define them to a clang annotate attribute
  std::string IsGeneratingMacro;
  std::vector<std::string> IgnoredHeaders;
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

  BaseExportOptions() {
  }

  llvm::StringRef createFullPath(llvm::StringRef path, llvm::SmallString<256> &pathBuff);
  llvm::Error gatherFiles(std::vector<std::string> &files);
};

struct HeaderGroupOptions : BaseExportOptions {
  // optional
  std::string Name;
  // HeaderDirectories or Headers is required
  std::vector<std::string> HeaderDirectories;
  std::vector<std::string> SourceDirectories;
  llvm::Error gatherDirectoryFiles(std::vector<std::string> &files);
  llvm::Error gatherSourceFiles(std::vector<std::string> &files);
};

typedef llvm::DenseMap<llvm::sys::fs::UniqueID, BaseExportOptions*> FileOptionLookup;

struct RootExportOptions : BaseExportOptions {
  std::string ClangFormatFile;
  std::vector<HeaderGroupOptions> Groups;
};

class ExportOptions : public RootExportOptions {
public:
  ExportOptions();
  ~ExportOptions();

  static llvm::Error loadFromFile(llvm::StringRef path, ExportOptions& options);
  static llvm::Error loadFromDirectory(const std::string &path, ExportOptions& options);
  static bool directoryHasExportConfig(const std::string &path);

  void setOverridesAndDefaults(const BaseExportOptions &options);
  llvm::Error gatherAllFiles(std::vector<std::string> &allFiles, FileOptionLookup &fileOptions);
  std::vector<HeaderGroupOptions>& getGroups() { return Groups; }

  HeaderGroupOptions *getGroup(llvm::StringRef name) {
    for (auto& group : Groups) {
      if (name.compare_insensitive(group.Name) == 0)
        return &group;
    }
    return nullptr;
  }

  clang::format::FormatStyle* getClangFormatStyle();

  const std::string &getRootDirectory() { return RootDirectory; }
  void setRootDirectory(llvm::StringRef RootDirectory) { 
    this->RootDirectory = RootDirectory; 
    tryLoadClangFormatFile();
  }
  llvm::Error tryLoadClangFormatFile();

private:
  llvm::Error Load(llvm::StringRef text);
  std::string RootDirectory;
  bool ClangFormatValid;
  clang::format::FormatStyle ClangFormatStyle;
};

#endif
