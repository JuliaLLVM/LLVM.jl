cmake_minimum_required(VERSION 3.13.4)

SET(CMAKE_CXX_FLAGS "-Wall -fPIC -fno-rtti")

project(LLVMExtra
VERSION
    1.8
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

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
include_directories("include")
add_subdirectory(lib)
