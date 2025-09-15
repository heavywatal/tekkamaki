cmake_minimum_required(VERSION 3.28)
project($ENV{R_PACKAGE_NAME}
  VERSION 0.8.3
  LANGUAGES CXX)

include(CMakePrintHelpers)
include(FetchContent)
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)
cmake_print_variables(CMAKE_CURRENT_SOURCE_DIR)

set(git_tag "$ENV{TEKKA_GIT_TAG}")
if(NOT git_tag)
  set(git_tag 620a016e6f9dfc8e246c2c40033be3003e216525)
  set(find_package_args FIND_PACKAGE_ARGS ${PROJECT_VERSION})
endif()
FetchContent_Declare(tekka
  GIT_REPOSITORY https://github.com/heavywatal/tekka.git
  GIT_TAG ${git_tag}
  ${find_package_args}
)

set(CMAKE_BUILD_TYPE Release)
set(BUILD_TESTING OFF CACHE BOOL "")
FetchContent_MakeAvailable(tekka)
cmake_print_variables(tekka_FOUND tekka_DIR)
if(tekka_FOUND)
  set(exec_INSTALL_PREFIX "${tekka_DIR}/../../..")
  set(PARAMETERS_JSON "${exec_INSTALL_PREFIX}/share/tekka/parameters.json")
else()
  cmake_print_variables(tekka_SOURCE_DIR tekka_BINARY_DIR)
  set(exec_INSTALL_PREFIX "$ENV{R_LIBRARY_DIR}/${PROJECT_NAME}") # load_all()
  set(CMAKE_INSTALL_BINDIR "exec")
  set(PARAMETERS_JSON "${tekka_SOURCE_DIR}/util/parameters.json")
endif()

set(exec_path "${exec_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/tekka")
cmake_print_variables(exec_path)
file(READ "${PARAMETERS_JSON}" default_parameters_json_text)
configure_file("config.cpp.in" "${CMAKE_CURRENT_SOURCE_DIR}/config.cpp" @ONLY)
