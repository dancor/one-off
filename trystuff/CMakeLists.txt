cmake_minimum_required(VERSION 3.10)
project(trystuff)
add_executable(b a.c)
target_link_libraries(b -Wall -ffast-math -march=native -O3)
