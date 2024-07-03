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

#include <llvm/Support/Error.h>
#include <llvm/ADT/SmallVector.h>
#include <vector>
#include <string>

struct BaseExportOptions {
  // optional
  std::vector<std::string> HeaderFiles;
  std::string MacroHeader;
  // optional if a global export macro is defined
  std::string ExportMacro;         
  std::string ClassMacro;          
  // optional
  std::string ExternTemplateMacro;
  // optional
  std::string ExternCMacro;
  // optional
  std::string ExportTemplateMacro; 
  // optional, This is defined while parsing headers scan for targets for
  // export macros. This should cause the code to not define the export macros
  // so the tool can define them to a clang annotate attribute
  std::string IsGeneratingMacro;
  std::vector<std::string> IgnoredHeaders;
  std::vector<std::string> IgnoredClasses;
  std::vector<std::string> OtherExportMacros;
  bool ExportExternC;
  bool ExportSimpleClasses;

  bool IsRoot;

  BaseExportOptions() 
    : ExportExternC(false), ExportSimpleClasses(false), IsRoot(false) {
  }

  llvm::Error gatherFiles(llvm::StringRef rootDirectory, std::vector<std::string> & files);
};

struct HeaderGroupOptions : BaseExportOptions {
  // optional
  std::string Name;
  // HeaderDirectories or Headers is required
  std::vector<std::string> HeaderDirectories;
  llvm::Error gatherDirectoryFiles(llvm::StringRef rootDirectory, std::vector<std::string> &files);
};

typedef llvm::DenseMap<llvm::sys::fs::UniqueID, BaseExportOptions*> FileOptionLookup;

class ExportOptions : public BaseExportOptions {
public:
  static llvm::Error loadFromFile(llvm::StringRef path, ExportOptions& options);
  static llvm::Error loadFromDirectory(const std::string &path, ExportOptions& options);

  void setOverridesAndDefaults(const BaseExportOptions &options);
  llvm::Error gatherAllFiles(llvm::StringRef rootDirectory, std::vector<std::string> &allFiles, FileOptionLookup &fileOptions);
  std::vector<HeaderGroupOptions>& getGroups() { return Groups; }

private:
  llvm::Error Load(llvm::StringRef text);
  std::vector<HeaderGroupOptions> Groups;
};

#endif