cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(Meter-test)

enable_language(CXX)

include_directories(
  ${PROJECT_SOURCE_DIR}
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

set(imgui_backend_sources imgui_impl_glfw.cpp imgui_impl_opengl3.cpp)
set(imgui_backend_headers imgui_impl_glfw.h   imgui_impl_opengl3.h)

foreach(file ${imgui_backend_sources} ${imgui_backend_headers})
  configure_file(
    ${PROJECT_SOURCE_DIR}/imgui/examples/${file}
    ${PROJECT_SOURCE_DIR}/imgui/${file}
    COPYONLY
  )
endforeach(file)

file(GLOB med_sources ${PROJECT_SOURCE_DIR}/med/*.cc )
file(GLOB imgui_sources_glob ${PROJECT_SOURCE_DIR}/imgui/*.cpp )
set(imgui_sources ${imgui_sources_glob})

foreach(file ${imgui_backend_sources})
  set(imgui_sources ${imgui_sources} ${PROJECT_SOURCE_DIR}/imgui/${file})
endforeach(file)

file(GLOB meter_sources ${PROJECT_SOURCE_DIR}/Source/*.cc)
file(GLOB test_sources ${PROJECT_SOURCE_DIR}/Tests/*.cc )

add_executable(tests
  ${meter_sources}
  ${test_sources}
  ${GOOGLETEST_SOURCES}
)

add_executable(med
  ${meter_sources}
  ${med_sources}
  ${imgui_sources}
)

target_link_libraries(tests
  googletest
)

# -Wno-mismatched-tags -fuse-ld=gold

target_compile_options(med PRIVATE -fno-exceptions -std=c++2a -Werror -fsanitize=undefined -Wall)
target_link_options(med PRIVATE -fsanitize=undefined)
target_link_libraries(med GL glfw GLEW)

target_compile_options(tests PRIVATE -fexceptions -fprofile-arcs -ftest-coverage -pthread -std=c++2a -Werror -fsanitize=undefined -Wall -Wno-sign-compare)
target_link_options(tests PRIVATE -pthread -fprofile-arcs -ftest-coverage -fsanitize=undefined)

set(CMAKE_CXX_FLAGS_DEBUG   "-O0 -g3")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

include(CTest)
enable_testing()

add_test(unit ${PROJECT_BINARY_DIR}/tests)