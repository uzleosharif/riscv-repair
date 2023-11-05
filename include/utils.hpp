// SPDX-License-Identifier: MIT

#pragma once

#include <ranges>

namespace utils {

[[nodiscard]] constexpr inline auto contains(std::ranges::input_range auto const& container,
                                             auto const& element) noexcept -> bool {
  return (std::ranges::find(container, element) != std::end(container));
}

}  // namespace utils
