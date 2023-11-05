
// SPDX-License-Identifier: MIT

#include "riscv_repair.hpp"

#include <ranges>

#include <spdlog/spdlog.h>

#include "RISCVSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "utils.hpp"

// TODO(uzleo): check non-exceptness!!
// TODO(uzleo): profile and optimize for performance (on complex target
//              MachineFunctions)
// TODO(uzleo): lambda capturing by reference constness??
// TODO(uzleo): prefer minstr_it (moperand_it) over minstr (moperand)
//              where possible using mbb.instrs()
//              -> best to work with iterators for generic programming!!

// Implementation Notes:
// -----------------------

// NOTE: We implement fine-grained scheduling (FGS) currently for
// shadow-instructions. We believe, with REPAIR transformation, the
// benefit of using coarse-grained (CGS) scheme to detect
// control-flow errors is minimal over using FGS in contrast to
// non-REPAIR based transformation f shadow-instruction as shown
// in gZDC scheme (Section 5.1.2 of M. Didebahn thesis).
//
// The FGS scheme implementation is less complex (less bugs). Futher,
// it has better runtime overhead compared to CGS on real hardware /
// RTL processor models.

// NOTE: We assume the memory to be protected from soft-error corruption.
// In other words, a data value in memory can never be corrupted.
// This is ok assumption considering most of off-the-shelf memory
// devices available are equipped with ECC protection. Hence, we don't
// implement redundancy for memory data (as in EDDI).

namespace {

enum class LogLevel { kNone, kDebug, kAll };

constexpr llvm::Register kSp{llvm::RISCV::X2};
constexpr LogLevel kLogLevel{LogLevel::kDebug};

consteval auto CanLogDebug() noexcept -> bool {
  return (kLogLevel == LogLevel::kDebug);
}

// TODO(uzleo): an unordered-map could be preferred but requires defining hash function for llvm::Register type
//              a flat-map also looks interesting for this problem
const std::map<llvm::Register, std::size_t> kRegToIndex{
    {llvm::RISCV::X0, 0},   {llvm::RISCV::X1, 1},   {llvm::RISCV::X2, 2},   {llvm::RISCV::X3, 3},
    {llvm::RISCV::X4, 4},   {llvm::RISCV::X5, 5},   {llvm::RISCV::X6, 6},   {llvm::RISCV::X8, 7},
    {llvm::RISCV::X10, 8},  {llvm::RISCV::X11, 9},  {llvm::RISCV::X12, 10}, {llvm::RISCV::X13, 11},
    {llvm::RISCV::X14, 12}, {llvm::RISCV::X15, 13}, {llvm::RISCV::X16, 14}, {llvm::RISCV::X17, 15}};

auto InsertMoveInstruction(llvm::MachineBasicBlock& mbb, llvm::MachineBasicBlock::iterator insert_it,
                           llvm::Register dst_reg, llvm::Register src_reg) noexcept -> void {
  llvm::BuildMI(mbb, insert_it, insert_it->getDebugLoc(),
                mbb.getParent()->getSubtarget().getInstrInfo()->get(llvm::RISCV::ADDI))
      .addReg(dst_reg)
      .addReg(src_reg)
      .addImm(0);
}

constexpr auto GetFunctionName(const llvm::MachineInstr& minstr) noexcept -> std::string {
  return minstr.getOperand(0).getGlobal()->getName().str();
}

auto IsShadowInstruction(const llvm::MachineInstr& minstr) noexcept -> bool {
  return std::ranges::any_of(minstr.operands(), [](const llvm::MachineOperand& moperand) {
    if (moperand.isReg()) {
      return utils::contains(llvm::riscv::sihft::RepairPass::kShadowRegs, moperand.getReg());
    }
    return false;
  });
}

}  // namespace

namespace llvm::riscv::sihft {

// NOTE: In the implementation, we have opted for spdlog logger instead of
// built-in llvm outs(), dbgs() utilities because logging with formatted
// string is much easier than ostream objects.

RepairPass::RepairPass() noexcept : MachineFunctionPass{sMfpId} {
  // TODO(uzleo): configure via compile-time programming
  spdlog::set_level(spdlog::level::info);

  if constexpr (CanLogDebug()) {
    spdlog::set_level(spdlog::level::debug);
  }
}

auto RepairPass::getPassName() const noexcept -> StringRef {
  return "Repair Pass";
}

//auto RepairPass::Init(MachineFunction& mfunction) noexcept
//    -> std::expected<void, std::string_view> {
auto RepairPass::Init(MachineFunction& mfunction) noexcept {
  m_mfunction = std::make_unique<std::reference_wrapper<MachineFunction>>(mfunction);
}

//auto RepairPass::Duplicate() noexcept -> std::expected<void, std::string_view> {
auto RepairPass::Duplicate() noexcept {

  auto mbb_transformer{[this](auto& mbb) {
    std::ranges::for_each(mbb.instrs() | std::views::filter([](MachineBasicBlock::const_iterator minstr_it) {
                            // TODO(uzleo): function-calls that should not be duplicated like fopen
                            return (not(minstr_it->isCFIInstruction() or minstr_it->isReturn() or
                                        minstr_it->isBranch() or minstr_it->mayStore()));
                          }),
                          [this, &mbb](MachineBasicBlock::iterator minstr_it) {
                            auto* cloned_minstr{m_mfunction->get().CloneMachineInstr(&*minstr_it)};

                            // Transform all register-operands to shadow ones in cloned instruction.
                            std::ranges::for_each(
                                cloned_minstr->operands() | std::views::filter([](auto& moperand) {
                                  if (not moperand.isReg()) {
                                    return false;
                                  }
                                  //if (auto index{GetZeroBasedIndex(moperand.getReg())}; not index.has_value()) {
                                  if (not kRegToIndex.contains(moperand.getReg())) {
                                    spdlog::error("The register {} in moperand is not a primary register.",
                                                  moperand.getReg());
                                    return false;
                                  }

                                  return true;
                                }),
                                [this](auto& moperand) {
                                  // TODO(uzleo): apply repair transformation

                                  moperand.setReg(m_primary_to_shadow[kRegToIndex.at(moperand.getReg())]);
                                });

                            mbb.insert(minstr_it, cloned_minstr);
                          });
  }};

  std::ranges::for_each(m_mfunction->get(), mbb_transformer);
}

//auto RepairPass::PostDuplicate() noexcept
//    -> std::expected<void, std::string_view> {
auto RepairPass::PostDuplicate() noexcept {

  // (1) Within the entry basic-block, all live primary registers (e.g. SP etc.)
  // need to be copied into shadow counterparts.

  InsertMoveInstruction(m_mfunction->get().front(), std::begin(m_mfunction->get().front()),
                        m_primary_to_shadow[kRegToIndex.at(kSp)], kSp);
  // The live-in of entry basic-block need to be moved into shadow counterparts.
  std::ranges::for_each(m_mfunction->get().front().liveins() | std::views::filter([](const auto& regmask_pair) {
                          if (not kRegToIndex.contains(static_cast<Register>(regmask_pair.PhysReg))) {
                            spdlog::error("The register in regmask_pair is not a primary register.");
                            return false;
                          }

                          return true;
                        }),
                        [this](const auto& regmask_pair) {
                          InsertMoveInstruction(
                              m_mfunction->get().front(), std::begin(m_mfunction->get().front()),
                              m_primary_to_shadow[kRegToIndex.at(static_cast<Register>(regmask_pair.PhysReg))],
                              static_cast<Register>(regmask_pair.PhysReg));
                        });

  // (2) Before calling function that are not protected, we have to save context.
  // Only non-callee saved registeres need to be stacked
  // TODO(uzleo): do we really need this?? I feel with the chosen register reservation
  // scheme, only t2-t6 need to be spilled (if they are live and if their current
  // shadow is callee-saved reg) as are risk of being clobbered by non-DMR function.

  // (3) Setting context before going into a non-DMR function.
  // TODO(uzleo): constness of instr!!

  auto functioncall_livein_filter{[](const auto& moperand) {
    return (moperand.isReg() and moperand.isImplicit() and moperand.isUse());
  }};

  auto mbb_transformer{[this, &functioncall_livein_filter](auto& mbb) {
    std::ranges::for_each(mbb.instrs() | std::views::filter([&functioncall_livein_filter](auto& minstr) {
                            if (minstr.isCall() and kUnprotectedFunctionCalls.contains(GetFunctionName(minstr)) and
                                not IsShadowInstruction(minstr)) {

                              return std::ranges::any_of(minstr.operands(), functioncall_livein_filter);
                            }

                            return false;
                          }),
                          [this, &mbb, &functioncall_livein_filter](auto& minstr) {
                            std::ranges::for_each(minstr.operands() | std::views::filter(functioncall_livein_filter),
                                                  [this, &mbb, &minstr](const auto& moperand) {
                                                    InsertMoveInstruction(
                                                        mbb, minstr.getIterator(), moperand.getReg(),
                                                        m_primary_to_shadow[kRegToIndex.at(moperand.getReg())]);
                                                  });
                          });
  }};

  std::ranges::for_each(m_mfunction->get(), mbb_transformer);
}

// NOTE: Currently, we opt for a simpler design that doesn't cache already
// traversed program objects (e.g. cache all stores in a container). While
// being inefficient, this makes maintaining code much simpler. For example,
// consider a new store has been inserted after Init() traversal then
// you have to make sure it is inserted into cached stores container as well.

auto RepairPass::runOnMachineFunction(MachineFunction& mfunction) noexcept -> bool {
  if (sDisableRepair) {
    return false;
  }

  spdlog::info("Running RepairPass on MF: {}", mfunction.getName().str());

  // TODO(uzleo): monadic composition of function pipeline

  Init(mfunction);
  Duplicate();
  PostDuplicate();

  return true;
}

}  // namespace llvm::riscv::sihft
