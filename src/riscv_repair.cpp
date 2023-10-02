
// SPDX-License-Identifier: MIT

#include "riscv_repair.hpp"

#include <expected>
#include <ranges>
#include <set>
#include <unordered_set>

#include "RISCVSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"

#include <spdlog/spdlog.h>

// TODO(uzleo): check non-exceptness!!
// TODO(uzleo): profile and optimize for performance (on complex target
//              MachineFunctions)
// TODO(uzleo): lambda capturing by reference constness??
// TODO(uzleo): prefer minstr_it (moperand_it) over minstr (moperand)
//              where possible using mbb.instrs()
//              -> best to work with iterators for generic programming!!

namespace {

enum class LogLevel { kNone, kDebug, kAll };

constexpr auto GetZeroBasedIndex(llvm::Register reg) noexcept
    -> std::expected<std::size_t, std::string_view>;
auto InsertMoveInstruction(llvm::MachineBasicBlock& mbb,
                           llvm::MachineBasicBlock::iterator insert_it,
                           llvm::Register dst_reg,
                           llvm::Register src_reg) noexcept -> void;
constexpr auto GetFunctionName(const llvm::MachineInstr& minstr) noexcept
    -> std::string;
auto IsShadowInstruction(const llvm::MachineInstr& minstr) noexcept -> bool;

constexpr llvm::Register kSp{llvm::RISCV::X2};
// TODO(uzleo): can we do a unordered_set here??
const std::set<llvm::Register> kShadowRegisters{
    std::cbegin(llvm::riscv::sihft::RepairPass::kReservedRegFile),
    std::cend(llvm::riscv::sihft::RepairPass::kReservedRegFile)};
const std::unordered_set<std::string_view> kUnprotectedFunctionCalls{"printf"};
constexpr LogLevel kLogLevel{LogLevel::kDebug};

consteval auto CanLogDebug() noexcept -> bool {
  return (kLogLevel == LogLevel::kDebug);
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
  m_mfunction =
      std::make_unique<std::reference_wrapper<MachineFunction>>(mfunction);
}

//auto RepairPass::Duplicate() noexcept -> std::expected<void, std::string_view> {
auto RepairPass::Duplicate() noexcept {
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

  auto mbb_transformer{[this](auto& mbb) {
    std::ranges::for_each(
        mbb.instrs() |
            std::views::filter([](MachineBasicBlock::const_iterator minstr_it) {
              return (not(minstr_it->isCFIInstruction() or
                          minstr_it->isReturn() or minstr_it->isBranch()));
            }),
        [this, &mbb](MachineBasicBlock::iterator minstr_it) {
          auto* cloned_minstr{
              m_mfunction->get().CloneMachineInstr(&*minstr_it)};

          // Transform all register-operands to shadow ones in cloned instruction.
          std::ranges::for_each(
              cloned_minstr->operands() | std::views::filter([](auto&
                                                                    moperand) {
                if (not moperand.isReg()) {
                  return false;
                }
                if (auto index{GetZeroBasedIndex(moperand.getReg())};
                    not index.has_value()) {
                  spdlog::error(
                      "The register {} in moperand is not a primary register.",
                      moperand.getReg());
                  return false;
                }

                return true;
              }),
              [this](auto& moperand) {
                // TODO(uzleo): apply repair transformation

                moperand.setReg(
                    m_primary_to_shadow[GetZeroBasedIndex(moperand.getReg())
                                            .value()]);
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

  InsertMoveInstruction(
      m_mfunction->get().front(), std::begin(m_mfunction->get().front()),
      m_primary_to_shadow[GetZeroBasedIndex(kSp).value()], kSp);
  // The live-in of entry basic-block need to be moved into shadow counterparts.
  std::ranges::for_each(
      m_mfunction->get().front().liveins() |
          std::views::filter([](const auto& regmask_pair) {
            if (auto index{GetZeroBasedIndex(
                    static_cast<Register>(regmask_pair.PhysReg))};
                not index.has_value()) {
              spdlog::error(
                  "The register in regmask_pair is not a primary register.");
              return false;
            }

            return true;
          }),
      [this](const auto& regmask_pair) {
        InsertMoveInstruction(
            m_mfunction->get().front(), std::begin(m_mfunction->get().front()),
            m_primary_to_shadow[GetZeroBasedIndex(
                                    static_cast<Register>(regmask_pair.PhysReg))
                                    .value()],
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
    std::ranges::for_each(
        mbb.instrs() | std::views::filter([&functioncall_livein_filter](
                                              auto& minstr) {
          if (minstr.isCall() and
              kUnprotectedFunctionCalls.contains(GetFunctionName(minstr)) and
              not IsShadowInstruction(minstr)) {

            return std::ranges::any_of(minstr.operands(),
                                       functioncall_livein_filter);
          }

          return false;
        }),
        [this, &mbb, &functioncall_livein_filter](auto& minstr) {
          std::ranges::for_each(
              minstr.operands() |
                  std::views::filter(functioncall_livein_filter),
              [this, &mbb, &minstr](const auto& moperand) {
                InsertMoveInstruction(
                    mbb, minstr.getIterator(), moperand.getReg(),
                    m_primary_to_shadow[GetZeroBasedIndex(moperand.getReg())
                                            .value()]);
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

auto RepairPass::runOnMachineFunction(MachineFunction& mfunction) noexcept
    -> bool {
  spdlog::info("Running RepairPass on MF: {}", mfunction.getName().str());

  // TODO(uzleo): monadic composition of function pipeline

  Init(mfunction);
  Duplicate();
  PostDuplicate();

  return true;
}

}  // namespace llvm::riscv::sihft

namespace {

constexpr auto GetZeroBasedIndex(llvm::Register reg) noexcept
    -> std::expected<std::size_t, std::string_view> {
  switch (reg) {
    case llvm::RISCV::X0:
      return 0;
    case llvm::RISCV::X1:
      return 1;
    case llvm::RISCV::X2:
      return 2;
    case llvm::RISCV::X3:
      return 3;
    case llvm::RISCV::X4:
      return 4;
    case llvm::RISCV::X5:
      return 5;
    case llvm::RISCV::X6:
      return 6;
    case llvm::RISCV::X8:
      return 7;
    case llvm::RISCV::X10:
      return 8;
    case llvm::RISCV::X11:
      return 9;
    case llvm::RISCV::X12:
      return 10;
    case llvm::RISCV::X13:
      return 11;
    case llvm::RISCV::X14:
      return 12;
    case llvm::RISCV::X15:
      return 13;
    case llvm::RISCV::X16:
      return 14;
    case llvm::RISCV::X17:
      return 15;
    default: {
      return std::unexpected{"A non-primary register passed in."};
    }
  }
}

auto InsertMoveInstruction(llvm::MachineBasicBlock& mbb,
                           llvm::MachineBasicBlock::iterator insert_it,
                           llvm::Register dst_reg,
                           llvm::Register src_reg) noexcept -> void {
  llvm::BuildMI(
      mbb, insert_it, insert_it->getDebugLoc(),
      mbb.getParent()->getSubtarget().getInstrInfo()->get(llvm::RISCV::ADDI))
      .addReg(dst_reg)
      .addReg(src_reg)
      .addImm(0);
}

constexpr auto GetFunctionName(const llvm::MachineInstr& minstr) noexcept
    -> std::string {
  return minstr.getOperand(0).getGlobal()->getName().str();
}

auto IsShadowInstruction(const llvm::MachineInstr& minstr) noexcept -> bool {
  return std::ranges::any_of(
      minstr.operands(), [](const llvm::MachineOperand& moperand) {
        if (moperand.isReg()) {
          return (kShadowRegisters.contains(moperand.getReg()));
        }
        return false;
      });
}

}  // namespace
