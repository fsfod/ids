add_executable(idt
  idt.cc
  FixItRewriter2.cpp 
  ExportOptionsConfig.cpp)

target_compile_definitions(idt PRIVATE
  ${LLVM_DEFINITIONS})
target_compile_options(idt PRIVATE
  $<$<CXX_COMPILER_ID:MSVC>:/EHsc- /GR->
  $<$<CXX_COMPILER_ID:AppleClang>:-fno-exceptions -fno-rtti>
  $<$<CXX_COMPILER_ID:Clang>:-fno-exceptions -fno-rtti>
  $<$<CXX_COMPILER_ID:GNU>:-fno-exceptions -fno-rtti>)
target_include_directories(idt PRIVATE
  ${LLVM_INCLUDE_DIRS}
  ${CLANG_INCLUDE_DIRS})
target_link_libraries(idt PRIVATE
  clangRewriteFrontend
  clangTooling)
target_precompile_headers(idt PUBLIC idt_pch.h)

if(MSVC)
 target_link_options(idt PRIVATE /INCREMENTAL)
 endif()
