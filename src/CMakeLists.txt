cmake_minimum_required(VERSION 3.4)
project(tekkamaki
  VERSION 0.5.5
  LANGUAGES CXX)

message(STATUS "CMAKE_CURRENT_SOURCE_DIR: ${CMAKE_CURRENT_SOURCE_DIR}")
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)

find_package(tekka ${PROJECT_VERSION} QUIET)
if(tekka_FOUND)
  get_target_property(INCLUDEDIR tekka::tekka INTERFACE_INCLUDE_DIRECTORIES)
  get_filename_component(CMAKE_INSTALL_PREFIX ${INCLUDEDIR}/.. ABSOLUTE)
  install(CODE "message(STATUS ${tekka_DIR})") # to make target 'install'
else()
  set(package tekka)
  set(repo heavywatal/${package})
  if(${PROJECT_VERSION} VERSION_LESS 0.1.0)
    set(version HEAD)
    set(branch master)
  else()
    set(version ${PROJECT_VERSION})
    set(branch v${version})
  endif()
  set(subdir ${PROJECT_BINARY_DIR}/${package}-${version})
  if(NOT EXISTS ${subdir})
    find_package(Git)
    execute_process(COMMAND
      ${GIT_EXECUTABLE} clone --recursive --depth=1 --branch=${branch} https://github.com/${repo}.git ${subdir}
    )
  endif()
  message(STATUS "add_subdirectory(${subdir})")
  set(CMAKE_BUILD_TYPE Release)
  set(BUILD_EXECUTABLE OFF CACHE BOOL "")
  set(BUILD_TESTING OFF CACHE BOOL "")
  add_subdirectory(${subdir} ${subdir}/build)
endif()

if(${CMAKE_INSTALL_PREFIX} MATCHES linuxbrew)
  set(CMAKE_INSTALL_LIBDIR lib)
endif()
set(LIBDIR ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR})
set(CPPFLAGS "-I${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}")
set(LDFLAGS "-L${LIBDIR} -Wl,-rpath,${LIBDIR}")
set(LDLIBS "-ltekka")
message(STATUS "CPPFLAGS: ${CPPFLAGS}")
message(STATUS "LDFLAGS: ${LDFLAGS}")
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Makevars.in
  ${CMAKE_CURRENT_SOURCE_DIR}/Makevars @ONLY
)
