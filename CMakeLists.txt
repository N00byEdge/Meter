cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(Meter-test)

enable_language(CXX)

if(NOT (CMAKE_CXX_COMPILER_ID MATCHES MSVC)) #GCC
  set(CMAKE_CXX_FLAGS "-std=c++2a -Werror -fsanitize=address,undefined -Wall -Wno-mismatched-tags -Wno-sign-compare")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-arcs -ftest-coverage -fexceptions -fuse-ld=gold -pthread")
  set(CMAKE_CXX_FLAGS_DEBUG   "-O0 -g3")
  set(CMAKE_CXX_FLAGS_RELEASE "-O3")
endif()

include_directories(
  ${PROJECT_SOURCE_DIR}/Source
)

set(GOOGLETEST_ROOT
  external/googletest/googletest CACHE STRING "Google Test source root"
)

include_directories(
  ${PROJECT_SOURCE_DIR}/${GOOGLETEST_ROOT}
  ${PROJECT_SOURCE_DIR}/${GOOGLETEST_ROOT}/include
)


set(GOOGLETEST_SOURCES
  ${PROJECT_SOURCE_DIR}/${GOOGLETEST_ROOT}/src/gtest-all.cc
  ${PROJECT_SOURCE_DIR}/${GOOGLETEST_ROOT}/src/gtest_main.cc
)

foreach(_source ${GOOGLETEST_SOURCES})
  set_source_files_properties(${_source} PROPERTIES GENERATED 1)
endforeach()

add_library(googletest
  ${GOOGLETEST_SOURCES}
)

add_custom_command(
  OUTPUT ${PROJECT_SOURCE_DIR}/Source/Tokenizer.cc
  DEPENDS ${PROJECT_SOURCE_DIR}/Source/Tokenizer.re
  COMMAND re2c --input custom -o "Tokenizer.cc" Tokenizer.re
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/Source"
)

file(GLOB testsources
  ${PROJECT_SOURCE_DIR}/Tests/*.cc
  ${PROJECT_SOURCE_DIR}/Source/*.cc
)

add_executable(tests
  ${testsources}
  ${GOOGLETEST_SOURCES}
  ${PROJECT_SOURCE_DIR}/Source/Tokenizer.cc
)

target_link_libraries(tests
  googletest
)

include(CTest)
enable_testing()

add_test(unit ${PROJECT_BINARY_DIR}/tests)