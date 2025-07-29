cmake_minimum_required(VERSION 3.21)
project($ENV{R_PACKAGE_NAME}
  VERSION 0.8.2
  LANGUAGES CXX)

include(CMakePrintHelpers)
include(FetchContent)
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)
cmake_print_variables(CMAKE_CURRENT_SOURCE_DIR)

set(tag "$ENV{TEKKA_GIT_TAG}")
if(NOT tag)
  set(tag "v${PROJECT_VERSION}")
  find_package(tekka ${PROJECT_VERSION} QUIET)
  cmake_print_variables(tekka_FOUND tekka_DIR)
endif()
if(tekka_FOUND)
  set(exec_INSTALL_PREFIX "${tekka_DIR}/../../..")
  set(PARAMETERS_JSON "${exec_INSTALL_PREFIX}/share/tekka/parameters.json")
else()
  set(exec_INSTALL_PREFIX "$ENV{R_LIBRARY_DIR}/${PROJECT_NAME}") # load_all()
  set(FETCHCONTENT_SOURCE_DIR_TEKKA "$ENV{FETCHCONTENT_SOURCE_DIR_TEKKA}")
  if(FETCHCONTENT_SOURCE_DIR_TEKKA AND NOT DEFINED ENV{TEKKA_GIT_TAG})
    set(tag "HEAD")
  endif()
  cmake_print_variables(FETCHCONTENT_SOURCE_DIR_TEKKA tag)
  set(FETCHCONTENT_QUIET OFF)
  FetchContent_Declare(
    tekka
    GIT_REPOSITORY https://github.com/heavywatal/tekka.git
    GIT_TAG ${tag}
    GIT_SHALLOW ON
    # FIND_PACKAGE_ARGS  # 3.24
  )
  set(CMAKE_BUILD_TYPE Release)
  set(CMAKE_INSTALL_BINDIR "exec")
  set(BUILD_TESTING OFF CACHE BOOL "")
  FetchContent_MakeAvailable(tekka)
  cmake_print_variables(tekka_SOURCE_DIR tekka_BINARY_DIR)
  set(PARAMETERS_JSON "${tekka_SOURCE_DIR}/util/parameters.json")
endif()

set(R_PACKAGE_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/..")
set(exec_path "${exec_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/tekka")
cmake_print_variables(exec_path)
file(READ "${PARAMETERS_JSON}" default_parameters_json_text)
configure_file("config.cpp.in" "${R_PACKAGE_SOURCE}/src/config.cpp" @ONLY)
