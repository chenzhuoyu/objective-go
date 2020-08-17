#include "token.h"
#include "utils/repr.h"
#include "utils/format.h"

struct NoSplitDirective : public Token::Directive {
    NoSplitDirective() noexcept : Directive(Token::Pragma::NoSplit, "nosplit") {}
};

struct NoEscapeDirective : public Token::Directive {
    NoEscapeDirective() noexcept : Directive(Token::Pragma::NoEscape, "noescape") {}
};

const Token::Directive *Token::NoSplit  = newNoSplit();
const Token::Directive *Token::NoEscape = newNoEscape();

const Token::Directive *Token::newNoSplit()  noexcept { return new NoSplitDirective; }
const Token::Directive *Token::newNoEscape() noexcept { return new NoEscapeDirective; }

std::string Token::toString() const {
    switch (_type) {
        case Type::LF        : return "Token(LF)";
        case Type::End       : return "Token(End)";
        case Type::Int       : return fmt::sprintf("Token(Int:%ld)", _int);
        case Type::Name      : return fmt::sprintf("Token(Name:%s)", _string);
        case Type::Rune      : return fmt::sprintf("Token(Rune:%#x)", _int);
        case Type::Float     : return fmt::sprintf("Token(Float:%g)", _float);
        case Type::String    : return fmt::sprintf("Token(String:%s)", utils::repr(_string));
        case Type::Complex   : return fmt::sprintf("Token(Complex:%s)", _complex);
        case Type::Keyword   : return fmt::sprintf("Token(Keyword:%s)", utils::repr(_string));
        case Type::Operator  : return fmt::sprintf("Token(Operator:%s)", name(_op));
        case Type::Directive : return fmt::sprintf("Token(Directive:%s)", *_directive);
    }
}

const char *Token::name(Type val) {
    switch (val) {
        case Type::LF        : return "LF";
        case Type::End       : return "End";
        case Type::Int       : return "Int";
        case Type::Name      : return "Name";
        case Type::Rune      : return "Rune";
        case Type::Float     : return "Float";
        case Type::String    : return "String";
        case Type::Complex   : return "Complex";
        case Type::Keyword   : return "Keyword";
        case Type::Operator  : return "Operator";
        case Type::Directive : return "Directive";
    }
}

const char *Token::name(Keyword val) {
    switch (val) {
        case Keyword::Async      : return "async";
        case Keyword::Break      : return "break";
        case Keyword::Case       : return "case";
        case Keyword::Catch      : return "catch";
        case Keyword::Chan       : return "chan";
        case Keyword::Class      : return "class";
        case Keyword::Const      : return "const";
        case Keyword::Continue   : return "continue";
        case Keyword::Default    : return "default";
        case Keyword::Defer      : return "defer";
        case Keyword::Else       : return "else";
        case Keyword::Extends    : return "extends";
        case Keyword::Finally    : return "finally";
        case Keyword::For        : return "for";
        case Keyword::Func       : return "func";
        case Keyword::If         : return "if";
        case Keyword::Implements : return "implements";
        case Keyword::Import     : return "import";
        case Keyword::In         : return "in";
        case Keyword::Interface  : return "interface";
        case Keyword::Map        : return "map";
        case Keyword::New        : return "new";
        case Keyword::Package    : return "package";
        case Keyword::Return     : return "return";
        case Keyword::Select     : return "select";
        case Keyword::Struct     : return "struct";
        case Keyword::Switch     : return "switch";
        case Keyword::Template   : return "template";
        case Keyword::Throw      : return "throw";
        case Keyword::Try        : return "try";
        case Keyword::Type       : return "type";
        case Keyword::Var        : return "var";
    }
}

const char *Token::name(Operator val) {
    switch (val) {
        case Operator::Plus        : return "+";
        case Operator::Minus       : return "-";
        case Operator::Multiply    : return "*";
        case Operator::Divide      : return "/";
        case Operator::Modulo      : return "%";
        case Operator::BitAnd      : return "&";
        case Operator::BitOr       : return "|";
        case Operator::BitXor      : return "^";
        case Operator::BitInvert   : return "~";
        case Operator::LShift      : return "<<";
        case Operator::RShift      : return ">>";
        case Operator::InpAdd      : return "+=";
        case Operator::InpSubtract : return "-=";
        case Operator::InpMultiply : return "*=";
        case Operator::InpDivide   : return "/=";
        case Operator::InpModulo   : return "%=";
        case Operator::InpBitAnd   : return "&=";
        case Operator::InpBitOr    : return "|=";
        case Operator::InpBitXor   : return "^=";
        case Operator::InpBitClear : return "~=";
        case Operator::InpLShift   : return "<<";
        case Operator::InpRShift   : return ">>";
        case Operator::BoolAnd     : return "&&";
        case Operator::BoolOr      : return "||";
        case Operator::BoolNot     : return "!";
        case Operator::Lambda      : return "->";
        case Operator::SendReceive : return "<-";
        case Operator::Increment   : return "++";
        case Operator::Decrement   : return "--";
        case Operator::Eq          : return "==";
        case Operator::Lt          : return "<";
        case Operator::Gt          : return ">";
        case Operator::Neq         : return "!=";
        case Operator::Leq         : return "<=";
        case Operator::Geq         : return ">=";
        case Operator::VarAssign   : return "=";
        case Operator::VarDefine   : return ":=";
        case Operator::Ellipsis    : return "...";
        case Operator::LBracket    : return "(";
        case Operator::RBracket    : return ")";
        case Operator::LIndex      : return "[";
        case Operator::RIndex      : return "]";
        case Operator::LBlock      : return "{";
        case Operator::RBlock      : return "}";
        case Operator::Comma       : return ",";
        case Operator::Point       : return ".";
        case Operator::Semicolon   : return ";";
        case Operator::Colon       : return ":";
    }
}
