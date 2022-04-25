cmake_minimum_required(VERSION 3.21)
project(lab01 C)

set(CMAKE_C_STANDARD 11)

add_executable(lab01
        mylab.c
        pointerTests.c
        pointerTests2.c
        stringTests.c)
