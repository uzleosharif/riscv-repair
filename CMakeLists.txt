
cmake_minimum_required(VERSION 3.26)

project(riscv_repair CXX)

set(CMAKE_EXPORT_COMPILER_COMMANDS ON)

set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ -Wall -Wextra -Wpedantic")

add_llvm_component_library(LLVMRISCVRepair
  src/riscv_repair.cpp

  LINK_COMPONENTS
  Core
  Support
  CodeGen

  ADD_TO_COMPONENT
  RISCV
)
target_include_directories(LLVMRISCVRepair
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
  PRIVATE "${CMAKE_SOURCE_DIR}/lib/Target/RISCV/"
  PRIVATE "${CMAKE_BINARY_DIR}/lib/Target/RISCV/"
)
