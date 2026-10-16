set(SOURCES
  Analysis.cpp
  Atomics.cpp
  Core.cpp
  NewPM.cpp
)

if(NOT LLVMEXTRA_STANDALONE)
  add_library(LLVMExtra-${LLVM_VERSION_MAJOR} SHARED ${SOURCES})
  target_link_libraries(LLVMExtra-${LLVM_VERSION_MAJOR} LLVM)
  install(TARGETS LLVMExtra-${LLVM_VERSION_MAJOR} DESTINATION lib)
  return()
endif()

cmake_minimum_required(VERSION 3.19)   # for COMMAND_ERROR_IS_FATAL

# the static libraries come from llvm-config, which knows their dependencies and the system
# libraries they need
find_program(LLVM_CONFIG llvm-config HINTS ${LLVM_TOOLS_BINARY_DIR} REQUIRED)
function(llvm_config var)
  execute_process(COMMAND ${LLVM_CONFIG} --link-static ${ARGN}
                  OUTPUT_VARIABLE output OUTPUT_STRIP_TRAILING_WHITESPACE
                  COMMAND_ERROR_IS_FATAL ANY)
  separate_arguments(output NATIVE_COMMAND "${output}")
  set(${var} ${output} PARENT_SCOPE)
endfunction()

# the libraries that define the functions of the C API are linked in completely, as nothing
# in the library references most of them; the others only as far as they are needed. not
# every version of LLVM has every component, so only use those that this one has.
set(LLVMEXTRA_EXPORTED_COMPONENTS
    core bitreader bitwriter irreader linker passes analysis transformutils ipo
    instcombine scalaropts vectorize target codegen mc object remarks mcdisassembler
    orcjit orcdebugging mcjit interpreter executionengine inteljitevents perfjitevents
    support all-targets
    CACHE STRING "LLVM components whose C API the standalone library exports")
execute_process(COMMAND ${LLVM_CONFIG} --components
                OUTPUT_VARIABLE available_components OUTPUT_STRIP_TRAILING_WHITESPACE
                COMMAND_ERROR_IS_FATAL ANY)
separate_arguments(available_components NATIVE_COMMAND "${available_components}")
set(exported_components)
foreach(component ${LLVMEXTRA_EXPORTED_COMPONENTS})
  if(component IN_LIST available_components)
    list(APPEND exported_components ${component})
  endif()
endforeach()
llvm_config(exported_libs --libfiles ${exported_components})
llvm_config(all_libs --libfiles all)
llvm_config(system_libs --system-libs)

set(name LLVM-standalone-${LLVM_VERSION_MAJOR})
add_library(${name} SHARED ${SOURCES})
# LLVMExtra's header doesn't annotate the functions it exports, so it is compiled with the
# default visibility, and the list of exported symbols hides everything else. besides the C
# API, that list includes the functions that support JIT-compiled code, which LLVM's JIT
# looks up by name (`llvm_orc_*`).
target_compile_options(${name} PRIVATE -ffunction-sections -fdata-sections)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # unique symbols are global even when hidden, and would bind to another copy of LLVM
  target_compile_options(${name} PRIVATE -fno-gnu-unique)
endif()

if(APPLE)
  set(whole_archive)
  foreach(lib ${exported_libs})
    list(APPEND whole_archive "-Wl,-force_load,${lib}")
  endforeach()
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/exports.txt "_LLVM*\n_llvm_orc_*\n")
  target_link_libraries(${name} PRIVATE
    ${whole_archive} ${all_libs} ${system_libs}
    "-Wl,-exported_symbols_list,${CMAKE_CURRENT_BINARY_DIR}/exports.txt"
    -Wl,-dead_strip)
else()
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/exports.map "{ global: LLVM*; llvm_orc_*; local: *; };\n")
  target_link_libraries(${name} PRIVATE
    -Wl,--whole-archive ${exported_libs} -Wl,--no-whole-archive
    -Wl,--start-group ${all_libs} -Wl,--end-group ${system_libs}
    "-Wl,--version-script,${CMAKE_CURRENT_BINARY_DIR}/exports.map"
    -Wl,-Bsymbolic -Wl,--gc-sections -Wl,-z,defs)
endif()
install(TARGETS ${name} DESTINATION lib)
