cmake_minimum_required(VERSION 3.15)
project(tekkamaki
  VERSION 0.8.0
  LANGUAGES CXX)

include(CMakePrintHelpers)
include(FetchContent)
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)
cmake_print_variables(CMAKE_CURRENT_SOURCE_DIR)

set(TEKKA_GIT_TAG $ENV{TEKKA_GIT_TAG})
cmake_print_variables(TEKKA_GIT_TAG)
if(NOT TEKKA_GIT_TAG)
  find_package(tekka ${PROJECT_VERSION} QUIET)
  cmake_print_variables(tekka_FOUND tekka_DIR)
endif()
if(tekka_FOUND)
  set(CMAKE_INSTALL_PREFIX "${tekka_DIR}/../../..")
  cmake_print_variables(CMAKE_INSTALL_PREFIX)
  set(PARAMETERS_JSON "${CMAKE_INSTALL_PREFIX}/share/tekka/parameters.json")
  install(CODE "message(STATUS ${tekka_DIR})") # to make target 'install'
else()
  if(DEFINED ENV{FETCHCONTENT_SOURCE_DIR_TEKKA})
    set(FETCHCONTENT_SOURCE_DIR_TEKKA $ENV{FETCHCONTENT_SOURCE_DIR_TEKKA})
    set(tag "HEAD")
  else()
    set(tag "v${PROJECT_VERSION}")
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

set(BINDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}")
cmake_print_variables(BINDIR)
get_filename_component(R_PACKAGE_SOURCE ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
file(WRITE ${R_PACKAGE_SOURCE}/inst/path "${BINDIR}/tekka")
cmake_print_variables(PARAMETERS_JSON)
install(FILES ${PARAMETERS_JSON} DESTINATION "${R_PACKAGE_SOURCE}/inst/")
