#ifndef OBJECTIVE_GO_FORMAT_H
#define OBJECTIVE_GO_FORMAT_H

#include <string>
#include <complex>
#include <fmt/printf.h>

namespace std { // NOLINT(cert-dcl58-cpp)
    template <typename T, typename U>
    basic_ostream<T> &operator<<(basic_ostream<T> &os, const complex<U> &val) {
        return os << "complex(" << val.real() << "+" << val.imag() << "i)";
    }
}

#endif /* OBJECTIVE_GO_FORMAT_H */