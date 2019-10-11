cmake_minimum_required(VERSION 3.13)
project(cc_labs)

set(CMAKE_CXX_STANDARD 14)

file(GLOB_RECURSE SOURCE_FILES "src/*.cpp" "src/*.c" "src/generated/*.c")
add_executable(ansi_c ${SOURCE_FILES})
target_link_libraries(ansi_c "stdc++")