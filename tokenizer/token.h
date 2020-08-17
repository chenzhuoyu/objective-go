#ifndef OBJECTIVE_GO_TOKEN_H
#define OBJECTIVE_GO_TOKEN_H

#include <cstdint>
#include <utility>
#include <type_traits>

#include "utils/types.h"
#include "utils/format.h"
#include "utils/traits.h"

struct Token : public gc, public CanStringize {
    enum class Type {
        LF,
        End,
        Int,
        Name,
        Rune,
        Float,
        String,
        Complex,
        Keyword,
        Operator,
        Directive,
    };

public:
    enum class Keyword {
        Async,
        Break,
        Case,
        Catch,
        Chan,
        Class,
        Const,
        Continue,
        Default,
        Defer,
        Else,
        Extends,
        Finally,
        For,
        Func,
        If,
        Implements,
        Import,
        In,
        Interface,
        Map,
        New,
        Package,
        Return,
        Select,
        Struct,
        Switch,
        Template,
        Throw,
        Try,
        Type,
        Var,
    };

public:
    enum class Operator {
        Plus,
        Minus,
        Multiply,
        Divide,
        Modulo,
        BitAnd,
        BitOr,
        BitXor,
        BitInvert,
        LShift,
        RShift,
        InpAdd,
        InpSubtract,
        InpMultiply,
        InpDivide,
        InpModulo,
        InpBitAnd,
        InpBitOr,
        InpBitXor,
        InpBitClear,
        InpLShift,
        InpRShift,
        BoolAnd,
        BoolOr,
        BoolNot,
        Lambda,
        SendReceive,
        Increment,
        Decrement,
        Eq,
        Lt,
        Gt,
        Neq,
        Leq,
        Geq,
        VarAssign,
        VarDefine,
        Ellipsis,
        LBracket,
        RBracket,
        LIndex,
        RIndex,
        LBlock,
        RBlock,
        Comma,
        Point,
        Semicolon,
        Colon,
    };

public:
    enum class Pragma {
        NoSplit,
        NoEscape,
        LinkName,
    };

public:
    class Directive : public gc, public CanStringize {
        Pragma      _type;
        const char *_name;

    protected:
        Directive(Pragma type, const char *name) :
            _type(type),
            _name(name) {}

    public:
        [[nodiscard]] Pragma      type() const { return _type; }
        [[nodiscard]] const char *name() const { return _name; }

    public:
        [[nodiscard]] std::string toString() const override {
            return fmt::sprintf("#{%s}", _name);
        }
    };

public:
    class LinkName : public Directive {
        std::string _name;
        std::string _link;

    public:
        LinkName(std::string name, std::string link) :
            Directive(Pragma::LinkName, "linkname"),
            _name(std::move(name)),
            _link(std::move(link)) {}

    public:
        [[nodiscard]] std::string toString() const override {
            return "#{noescape}";
        }
    };

private:
    static const Directive *newNoSplit() noexcept;
    static const Directive *newNoEscape() noexcept;

public:
    static const Directive *NoSplit;
    static const Directive *NoEscape;

private:
    int         _col;
    int         _row;
    Type        _type;
    std::string _file;

private:
    Operator          _op;
    int64_t           _int;
    bool              _bool;
    double            _float;
    Bytes             _bytes;
    std::string       _string;
    Complex           _complex;
    Keyword           _keyword;
    const Directive * _directive;

public:
    [[nodiscard]] int         col()  const { return _col; }
    [[nodiscard]] int         row()  const { return _row; }
    [[nodiscard]] std::string file() const { return _file; }

public:
    template <typename T>
    [[nodiscard]] inline T value() const {
        static constexpr bool gettable = sizeof(T) == 0;
        static_assert(gettable, "Cannot get value of type T.");
    }

public:
    template <> [[nodiscard]] Operator          value() const { return _op; }
    template <> [[nodiscard]] int64_t           value() const { return _int; }
    template <> [[nodiscard]] bool              value() const { return _bool; }
    template <> [[nodiscard]] double            value() const { return _float; }
    template <> [[nodiscard]] Bytes             value() const { return _bytes; }
    template <> [[nodiscard]] std::string       value() const { return _string; }
    template <> [[nodiscard]] Complex           value() const { return _complex; }
    template <> [[nodiscard]] Keyword           value() const { return _keyword; }
    template <> [[nodiscard]] const Directive * value() const { return _directive; }

public:
    [[nodiscard]] std::string toString() const override;

public:
    static const char *name(Type val);
    static const char *name(Keyword val);
    static const char *name(Operator val);

};

template <typename T>
std::basic_ostream<T> &operator <<(std::basic_ostream<T> &os, Token::Type val) {
    return os << Token::name(val);
}

template <typename T>
std::basic_ostream<T> &operator <<(std::basic_ostream<T> &os, Token::Keyword val) {
    return os << Token::name(val);
}

template <typename T>
std::basic_ostream<T> &operator <<(std::basic_ostream<T> &os, Token::Operator val) {
    return os << Token::name(val);
}

#endif /* OBJECTIVE_GO_TOKEN_H */
