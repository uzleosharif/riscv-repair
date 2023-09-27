
// SPDX-License-Identifier: MIT

#include "riscv_repair.hpp"

#include <print>

namespace riscv::sihft {

RepairPass::RepairPass() noexcept : llvm::MachineFunctionPass{kMfpId} {}

auto RepairPass::getPassName() const noexcept -> llvm::StringRef {
  return "Repair Pass";
}

auto RepairPass::runOnMachineFunction(llvm::MachineFunction& mfunction) noexcept
    -> bool {
  std::println("Running RepairPass on MF");

  return true;
}

}  // namespace riscv::sihft
