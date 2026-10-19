cmake_minimum_required(VERSION 3.13.4)

# match LLVM, which is built without RTTI and exceptions. without -fno-exceptions, GCC 14+
# makes optimized code call __cxa_call_terminate, which the libstdc++ that Julia 1.10 and
# 1.11 ship on Windows doesn't export, so the library fails to load there.
SET(CMAKE_CXX_FLAGS "-Wall -fPIC -fno-rtti -fno-exceptions")

project(LLVMExtra
VERSION
    1.11
LANGUAGES
   CXX
   C
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


find_package(LLVM REQUIRED CONFIG)
set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  ${LLVM_CMAKE_DIR}
)
include(AddLLVM)
include(HandleLLVMOptions)

message(STATUS "Found LLVM: ${LLVM_VERSION}")

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
include_directories(${LLVM_INCLUDE_DIRS})
include_directories("include")
add_subdirectory(lib)
