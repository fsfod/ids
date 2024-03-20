add_executable(idt
  idt.cc)
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
if(MSVC)

file(READ idt.cc CONTENTS)

string(FIND "${CONTENTS}" "#pragma hdrstop" HDRSTOP_POS)

if(HDRSTOP_POS EQUAL  -1)
    message(FATAL_ERROR "#pragma hdrstop not found in idt_pch.cpp")
endif()

# Extract includes up to #pragma hdrstop
math(EXPR HDRSTOP_END_POS "${HDRSTOP_POS}") 
string(SUBSTRING "${CONTENTS}" 0 ${HDRSTOP_END_POS} CONTENTS_UP_TO_HDRSTOP)

file(READ idt_pch.cpp OLD_PCHSRC)

# Don't overwrite the PCH source file if includes haven't changed
if(NOT OLD_PCHSRC OR NOT "${OLD_PCHSRC}" STREQUAL "${CONTENTS_UP_TO_HDRSTOP}")
  file(WRITE idt_pch.cpp "${CONTENTS_UP_TO_HDRSTOP}")
  message(STATUS "Regenerated precompiled header source file idt_pch.cpp")
endif()

set(PCH_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/idt_pch.dir/idt_pch.pch")
set(PCHOBJ_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/idt_pch.obj")

add_library(idt_pch OBJECT idt_pch.cpp)
set_source_files_properties("idt_pch.cpp" PROPERTIES OBJECT_OUTPUTS ${PCH_OUTPUT})

target_include_directories(idt_pch PRIVATE $<TARGET_PROPERTY:idt,INCLUDE_DIRECTORIES>)
target_compile_definitions(idt_pch PRIVATE $<TARGET_PROPERTY:idt,COMPILE_DEFINITIONS>)
target_compile_features(idt_pch PRIVATE $<TARGET_PROPERTY:idt,COMPILE_FEATURES>)
target_compile_options(idt_pch PRIVATE $<LIST:REMOVE_ITEM,$<TARGET_PROPERTY:idt,COMPILE_OPTIONS>,/Yu>)
set_target_properties(idt_pch PROPERTIES COMPILE_FLAGS /Yc)
# The PCH needs to share the same PDB as the source file
set_target_properties(idt_pch PROPERTIES COMPILE_PDB_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/idt.dir)

set_source_files_properties(idt.cc PROPERTIES OBJECT_DEPENDS ${PCH_OUTPUT})
add_dependencies(idt idt_pch)

target_compile_options(idt PRIVATE /Yu /Fp${PCH_OUTPUT})
target_link_libraries(idt PRIVATE $<TARGET_OBJECTS:idt_pch>)
