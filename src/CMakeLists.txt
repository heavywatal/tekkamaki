cmake_minimum_required(VERSION 3.28)
project($ENV{R_PACKAGE_NAME}
  VERSION 0.9.0
  LANGUAGES CXX)

include(CMakePrintHelpers)
include(FetchContent)
include(GNUInstallDirs)
set(CMAKE_VERBOSE_MAKEFILE ON)
cmake_print_variables(CMAKE_CURRENT_SOURCE_DIR)

set(git_tag "$ENV{TEKKA_GIT_TAG}")
if(NOT git_tag)
  set(git_tag 533da8e567c810805c43a2511ef4252ab6d6a79d)
  set(find_package_args FIND_PACKAGE_ARGS ${PROJECT_VERSION})
endif()
FetchContent_Declare(tekka
  GIT_REPOSITORY https://github.com/heavywatal/tekka.git
  GIT_TAG ${git_tag}
  ${find_package_args}
)

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
