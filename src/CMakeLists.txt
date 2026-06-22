cmake_minimum_required(VERSION 3.28)
project($ENV{R_PACKAGE_NAME}
  VERSION 0.10.1
  LANGUAGES CXX)

include(CMakePrintHelpers)
include(FetchContent)
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)
cmake_print_variables(CMAKE_CURRENT_SOURCE_DIR)

set(repo_url https://github.com/heavywatal/tekka)
if(DEFINED ENV{TEKKA_GIT_TAG})
  FetchContent_Declare(tekka
    GIT_REPOSITORY ${repo_url}.git
    GIT_TAG $ENV{TEKKA_GIT_TAG}
  )
else()
  FetchContent_Declare(tekka
    URL ${repo_url}/archive/refs/tags/v${PROJECT_VERSION}.tar.gz
    URL_HASH SHA256=d74e7e9e94d770ff9fff570c3ff31dfd5170ed0aa422ccf99964889d0e6c1f16
    FIND_PACKAGE_ARGS ${PROJECT_VERSION}
  )
endif()

set(CMAKE_BUILD_TYPE Release)
set(CMAKE_INSTALL_BINDIR "exec")
FetchContent_MakeAvailable(tekka)

cmake_print_variables(tekka_FOUND tekka_DIR)
if(tekka_FOUND)
  get_property(_cfg TARGET tekka::tekka-exe PROPERTY IMPORTED_CONFIGURATIONS)
  get_property(exec_path TARGET tekka::tekka-exe PROPERTY IMPORTED_LOCATION_${_cfg})
else()
  cmake_print_variables(tekka_SOURCE_DIR tekka_BINARY_DIR)
  set(exec_path "$ENV{R_LIBRARY_DIR}/${PROJECT_NAME}/exec/tekka")
endif()

cmake_print_variables(exec_path)
configure_file("config.cpp.in" "${CMAKE_CURRENT_SOURCE_DIR}/config.cpp" @ONLY)

cmake_print_variables(CMAKE_CURRENT_BINARY_DIR)
if("${CMAKE_CURRENT_BINARY_DIR}" MATCHES "/build$")
  add_library(compile_commands OBJECT config.cpp cpp11.cpp)
  target_include_directories(compile_commands PRIVATE "$ENV{R_INCLUDE_DIR}")
  string(REPLACE ":" ";" R_LIBS_USER "$ENV{R_LIBS_USER}")
  string(REPLACE ":" ";" R_LIBS_SITE "$ENV{R_LIBS_SITE}")
  set(R_HOME "$ENV{R_HOME}/library")
  foreach(r_lib IN LISTS R_LIBS_USER R_LIBS_SITE R_HOME)
    if(IS_DIRECTORY "${r_lib}/cpp11/include")
      target_include_directories(compile_commands PRIVATE "${r_lib}/cpp11/include")
    endif()
  endforeach()
endif()
