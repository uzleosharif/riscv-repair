

# RISCV-REPAIR

Implementation of REPAIR SIHFT method for RISCV ISA. 

## Developer Notes
- This is meant to be placed inside the `llvm-project/llvm/lib/Target/RISCV/` 
location so that this back-end (`MachineFunctionPass`) can be built and 
made available to be link against for the `llc` compiler. In other words,
this project is not meant to be built as standalone library/application.

- `CMMakeLists.txt` expects the `LLVM_PROJECT_DIR` variable to point to 
`llvm-project` install location on your system.
