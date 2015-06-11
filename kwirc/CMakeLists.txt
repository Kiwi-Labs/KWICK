cmake_minimum_required (VERSION 3.0.2)
project (KIRC)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${KIRC_SOURCE_DIR}/inc)
add_definitions(${LLVM_DEFINITIONS})

set(RESOURCE_COMPILER xxd)
file(GLOB_RECURSE COMPILED_RESOURCES "llraw/*")
foreach(INPUT_FILE_PATH ${COMPILED_RESOURCES})
    get_filename_component(OUTPUT_FILE_NAME ${INPUT_FILE_PATH} NAME)
    set(OUTPUT_FILE ${CMAKE_SOURCE_DIR}/llinc/${OUTPUT_FILE_NAME}.h)
    add_custom_command(
        OUTPUT ${OUTPUT_FILE}
        COMMAND ${RESOURCE_COMPILER} -i ${INPUT_FILE_PATH} ${OUTPUT_FILE}
        COMMENT "Compiling ${INPUT_FILE_PATH} to includable hex array.")
    list(APPEND COMPILED_RESOURCES ${OUTPUT_FILE})
endforeach()

file(GLOB KIRC_SRC lib/*.cpp)

# Kiwi IR Compiler. To eventually be linked in as a statically linked library, but starting as an executable for testing.
set (KIRC_VERSION_MAJOR 0)
set (KIRC_VERSION_MINOR 1)

set(CLANG_CXX11_STD_FLAG "-std=c++11")
set(CLANG_CXX11_STDLIB_FLAG "-stdlib=libc++")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CLANG_CXX11_STD_FLAG} ${CLANG_CXX11_STDLIB_FLAG}")

set (KIRC_SOURCES main.cpp)

add_executable(KIRC main.cpp ${KIRC_SRC} ${COMPILED_RESOURCES})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs all)

# Link against LLVM libraries
target_link_libraries(KIRC ${llvm_libs})

add_custom_command(
  OUTPUT runtimeoutput
  COMMAND KIRC
  COMMENT "Running built executable"
  VERBATIM
)

add_custom_target(
  KIRC_run ALL
  DEPENDS runtimeoutput
)