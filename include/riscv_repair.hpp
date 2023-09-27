// SPDX-Liecense-Identifier: MIT

#pragma once

#include <llvm/CodeGen/MachineFunctionPass.h>

namespace riscv::sihft {

/// The back-end pass to implement REPAIR SIHFT method on LLVM IR code.
class RepairPass : public llvm::MachineFunctionPass {
 public:
  static inline char kMfpId{0};

  RepairPass() noexcept;
  RepairPass(const RepairPass&) = delete;
  RepairPass(RepairPass&&) = delete;
  auto operator=(const RepairPass&) -> RepairPass& = delete;
  auto operator=(RepairPass&&) -> RepairPass& = delete;

  ~RepairPass() override = default;

  [[nodiscard]] auto runOnMachineFunction(
      llvm::MachineFunction& mfunction) noexcept -> bool override;
  [[nodiscard]] auto getPassName() const noexcept -> llvm::StringRef override;
};

}  // namespace riscv::sihft
