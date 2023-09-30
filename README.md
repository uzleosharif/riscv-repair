

# RISCV-REPAIR

Implementation of REPAIR SIHFT method for RISCV ISA. 

## Developer Notes
- This is meant to be placed inside the `llvm-project/llvm/lib/Target/RISCV/` 
location so that this back-end (`MachineFunctionPass`) can be built and 
made available to be link against for the `llc` compiler. In other words,
this project is not meant to be built as standalone library/application.

## Dependencies
- Install `spdlog` library (>v1.12) somewhere in system paths as a shared 
library. Ideally build this library using `clang++-17` compiler with 
`-stdlib=libc++`.

