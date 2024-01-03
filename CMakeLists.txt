cmake_minimum_required(VERSION 3.15.0)
project(tmp)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(${PROJECT_SOURCE_DIR}/turing)

aux_source_directory(${PROJECT_SOURCE_DIR}/turing C_SRC)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)
add_executable(turing ${C_SRC})
