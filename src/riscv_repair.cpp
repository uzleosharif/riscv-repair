
// SPDX-License-Identifier: MIT

#include "riscv_repair.hpp"

#include <spdlog/spdlog.h>

namespace riscv::sihft {

RepairPass::RepairPass() noexcept : llvm::MachineFunctionPass{kMfpId} {
  spdlog::set_level(spdlog::level::info);
}

auto RepairPass::getPassName() const noexcept -> llvm::StringRef {
  return "Repair Pass";
}

auto RepairPass::runOnMachineFunction(llvm::MachineFunction& mfunction) noexcept
    -> bool {
  spdlog::info("Running RepairPass on MF.");

  return true;
}

}  // namespace riscv::sihft
