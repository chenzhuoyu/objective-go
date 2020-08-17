#ifndef OBJECTIVE_GO_TRAITS_H
#define OBJECTIVE_GO_TRAITS_H

#include <string>
#include <typeinfo>
#include <type_traits>

#include "format.h"

/** Type Mixins **/

struct Immovable {
    Immovable() = default;
    Immovable(Immovable &&) = delete;
    Immovable &operator=(Immovable &&) = delete;
};

struct NonCopyable {
    NonCopyable() = default;
    NonCopyable(const NonCopyable &) = delete;
    NonCopyable &operator=(const NonCopyable &) = delete;
};

struct CanStringize {
    [[nodiscard]] virtual std::string toString() const {
        return fmt::sprintf(
            "<%s object at %p>",
            typeid(*this).name(),
            reinterpret_cast<const void *>(this)
        );
    }
};

/** Formatter Integrations **/

template <typename T>
struct [[maybe_unused]] fmt::formatter<
    T,
    char,
    std::enable_if_t<
        std::is_base_of_v<CanStringize, T>,
        void
    >
> : public fmt::formatter<std::string> {
    template <typename U>
    [[maybe_unused]] auto format(const CanStringize &val, U &ctx) {
        return fmt::formatter<std::string>::format(val.toString(), ctx);
    }
};

template <typename T>
std::basic_ostream<T> &operator <<(std::basic_ostream<T> &os, const CanStringize &value) {
    return os << value.toString();
}

#endif /* OBJECTIVE_GO_TRAITS_H */
