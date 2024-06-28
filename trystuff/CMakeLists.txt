cmake_minimum_required(VERSION 3.10)
project(trystuff)
add_executable(a b.c)
target_link_libraries(a -Wall -ffast-math -march=native -O3)
