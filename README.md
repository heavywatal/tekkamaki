# tekkamaki <img src="man/figures/logo.svg" align="right" height=140/>

[![R-CMD-check](https://github.com/heavywatal/tekkamaki/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heavywatal/tekkamaki/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/heavywatal/tekkamaki/graph/badge.svg?token=IRpj1vyfrZ)](https://codecov.io/gh/heavywatal/tekkamaki)

R interface to [tekka](https://github.com/heavywatal/tekka).

## Requirements

- Unix-like OS (macOS, Linux, etc.)
- C++17 compiler (clang++ >= Apple LLVM 12, g++ >= 8)
- [CMake](https://cmake.org/) (>= 3.15.0)

## Installation

```r
# install.packages("pak")
pak::pkg_install("heavywatal/tekkamaki")
```

The code above tries to find and use an installed `tekka` in your system.
If it is too old or not found, the latest version will be installed.

To use a bleeding edge or some specific revision, set the environment variable `TEKKA_GIT_TAG` before installation:
```r
Sys.setenv(TEKKA_GIT_TAG = "HEAD")
```
