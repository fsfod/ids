//===- ExportOptionsConfig.h - Config file system for export macros*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This is a diagnostic client adaptor that performs rewrites as
//
//===----------------------------------------------------------------------===//

#ifndef EXPORT_OPTIONS_CONFIG_H
#define EXPORT_OPTIONS_CONFIG_H

#include <vector>
#include <string>
#include <llvm/Support/Error.h>

enum ExportOptionFlags {
  ExportOptions_None  = 0,
  ExportExternC       = 1,
  ExportSimpleClasses = 2,
};

struct BaseExportOptions {
  // optional
  std::vector<std::string> Headers;
  std::string MacroHeader;
  // optional          
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
  std::vector<std::string> ExtraExportMacros;
  bool ExportExternC;
  bool ExportSimpleClasses;

  bool IsRoot;

  BaseExportOptions() 
    : ExportExternC(false), ExportSimpleClasses(false) {
  }
};

struct HeaderGroupOptions : BaseExportOptions {
  // optional
  std::string Name;
    // HeaderDir or Headers is required
  std::string HeaderDir; 
  std::string HeaderDirectory;
  std::vector<std::string> HeaderFiles;
};

class ExportOptions : public BaseExportOptions {
public:
  void PropagateDefaults(const std::string &exportMacro = "");
  static llvm::Error LoadFromFile(llvm::StringRef path, ExportOptions& options);
  static llvm::Error LoadFromDirectory(const std::string &path, ExportOptions& options);

  std::vector<HeaderGroupOptions>& GetGroups() { return Groups; }

private:
  llvm::Error Load(llvm::StringRef text);
  std::vector<HeaderGroupOptions> Groups;
};

#endif