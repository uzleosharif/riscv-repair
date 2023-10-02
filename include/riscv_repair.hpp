// SPDX-Liecense-Identifier: MIT

#pragma once

#include <llvm/CodeGen/MachineFunctionPass.h>
#include "RISCV.h"

//#include <expected>

namespace llvm::riscv {

/// There are 32 General-Purpose Registers in RISCV ISAs.
constexpr inline std::size_t kNumGprRegs{32};
constexpr inline std::size_t kNumGprRegsHalf{kNumGprRegs / 2};

}  // namespace llvm::riscv

namespace llvm::riscv::sihft {

/// The back-end pass to implement REPAIR SIHFT method on LLVM IR code.
class RepairPass : public MachineFunctionPass {
 public:
  static inline char sMfpId{0};

  // primary-registers:
  // - {zero, sp, ra, tp, gp, fp/s0} are used by compiler for special uses
  // - {a0-a7} are arg/return registers as per calling-convention
  // - {t0-t1} are chosen so as to have all callee-saved regs as shadow regs
  //
  // shadow-registers:
  // - {s1-s11} are callee-saved registers
  // - {t2-t6} are remaining registers
  constexpr static std::array<llvm::Register, kNumGprRegsHalf> kReservedRegFile{
      llvm::RISCV::X7,  llvm::RISCV::X9,  llvm::RISCV::X18, llvm::RISCV::X19,
      llvm::RISCV::X20, llvm::RISCV::X21, llvm::RISCV::X22, llvm::RISCV::X23,
      llvm::RISCV::X24, llvm::RISCV::X25, llvm::RISCV::X26, llvm::RISCV::X27,
      llvm::RISCV::X28, llvm::RISCV::X29, llvm::RISCV::X30, llvm::RISCV::X31};

  RepairPass() noexcept;
  RepairPass(const RepairPass&) = delete;
  RepairPass(RepairPass&&) = delete;
  auto operator=(const RepairPass&) -> RepairPass& = delete;
  auto operator=(RepairPass&&) -> RepairPass& = delete;

  ~RepairPass() noexcept override = default;

  [[nodiscard]] auto runOnMachineFunction(MachineFunction& mfunction) noexcept
      -> bool override;
  [[nodiscard]] auto getPassName() const noexcept -> StringRef override;

 private:
  // TODO: error-codes instead of string_view

  // NOTE: The client (llc) should be compiled with c++23 to make use of expected
  // interface
  /*
  auto Init(MachineFunction& mfunction) noexcept
      -> std::expected<void, std::string_view>;
  auto Duplicate() noexcept -> std::expected<void, std::string_view>;
  auto PostDuplicate() noexcept -> std::expected<void, std::string_view>;
  */

  auto Init(MachineFunction& mfunction) noexcept;
  auto Duplicate() noexcept;
  auto PostDuplicate() noexcept;

  std::unique_ptr<std::reference_wrapper<MachineFunction>> m_mfunction{nullptr};
  std::array<Register, kNumGprRegsHalf> m_primary_to_shadow{kReservedRegFile};
};

static_assert(not(std::is_copy_assignable_v<RepairPass> or
                  std::is_copy_constructible_v<RepairPass> or
                  std::is_move_assignable_v<RepairPass> or
                  std::is_move_constructible_v<RepairPass>));

}  // namespace llvm::riscv::sihft
