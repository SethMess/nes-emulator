cmake_minimum_required(VERSION 3.16)

# Project name + language
project(NesEmulator VERSION 0.1 LANGUAGES CXX)

# Use modern C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find SDL2 using pkg-config (MSYS2 makes this work)
find_package(SDL2 REQUIRED)
include_directories(${SDL2_INCLUDE_DIRS})

# Add source files
add_executable(nes
    src/main.cpp
    src/cpu.cpp
    # src/ppu.cpp
    src/bus.cpp
    # src/cartridge.cpp
)

# target_link_libraries(nes_emulator ${SDL2_LIBRARIES})

# Add include directory for headers
target_include_directories(nes PRIVATE include)

# Find SDL2 (system-installed or via package manager)
find_package(SDL2 REQUIRED)
target_link_libraries(nes PRIVATE SDL2::SDL2)
