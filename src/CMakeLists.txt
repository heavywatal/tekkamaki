cmake_minimum_required(VERSION 3.15)
project(tekkamaki
  VERSION 0.7.2
  LANGUAGES CXX)

include(CMakePrintHelpers)
include(FetchContent)
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)
cmake_print_variables(CMAKE_CURRENT_SOURCE_DIR)

find_package(tekka ${PROJECT_VERSION} QUIET)
cmake_print_variables(tekka_FOUND tekka_DIR)
if(tekka_FOUND)
  get_target_property(INCLUDEDIR tekka::tekka INTERFACE_INCLUDE_DIRECTORIES)
  get_filename_component(CMAKE_INSTALL_PREFIX "${INCLUDEDIR}" DIRECTORY)
  cmake_print_variables(CMAKE_INSTALL_PREFIX)
  if(${CMAKE_INSTALL_PREFIX} MATCHES linuxbrew)
    set(CMAKE_INSTALL_LIBDIR lib)
  endif()
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
  set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)
  set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)
  set(CMAKE_INSTALL_BINDIR "exec")
  set(CMAKE_INSTALL_LIBDIR "libs")
  set(BUILD_EXECUTABLE ON CACHE BOOL "")
  set(BUILD_TESTING OFF CACHE BOOL "")
  FetchContent_MakeAvailable(tekka)
  cmake_print_variables(tekka_SOURCE_DIR tekka_BINARY_DIR)
endif()

set(BINDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}")
set(LIBDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
set(CPPFLAGS "-I${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}")
set(LDFLAGS "-L${LIBDIR} -Wl,-rpath,${LIBDIR}")
set(LDLIBS "-ltekka")
cmake_print_variables(CPPFLAGS)
cmake_print_variables(LDFLAGS)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Makevars.in
  ${CMAKE_CURRENT_SOURCE_DIR}/Makevars @ONLY
)
get_filename_component(R_PACKAGE_SOURCE ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
file(WRITE ${R_PACKAGE_SOURCE}/inst/path "${BINDIR}/tekka")
