cmake_minimum_required(VERSION 2.8)
project(fortraquarium)
enable_language(Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod")

file(GLOB sources *.f90)

add_executable("fortraquarium" ${sources})
