#include "repr.h"

static const char *HexTable[256] = {
    R"(\x00)", R"(\x01)", R"(\x02)", R"(\x03)", R"(\x04)", R"(\x05)", R"(\x06)", R"(\x07)",
    R"(\x08)", R"(\t)"  , R"(\n)"  , R"(\x0b)", R"(\x0c)", R"(\r)"  , R"(\x0e)", R"(\x0f)",
    R"(\x10)", R"(\x11)", R"(\x12)", R"(\x13)", R"(\x14)", R"(\x15)", R"(\x16)", R"(\x17)",
    R"(\x18)", R"(\x19)", R"(\x1a)", R"(\x1b)", R"(\x1c)", R"(\x1d)", R"(\x1e)", R"(\x1f)",
    " "      , "!"      , R"(\")"  , "#"      , "$"      , "%"      , "&"      , "'"      ,
    "("      , ")"      , "*"      , "+"      , ","      , "-"      , "."      , "/"      ,
    "0"      , "1"      , "2"      , "3"      , "4"      , "5"      , "6"      , "7"      ,
    "8"      , "9"      , ":"      , ";"      , "<"      , "="      , ">"      , "?"      ,
    "@"      , "A"      , "B"      , "C"      , "D"      , "E"      , "F"      , "G"      ,
    "H"      , "I"      , "J"      , "K"      , "L"      , "M"      , "N"      , "O"      ,
    "P"      , "Q"      , "R"      , "S"      , "T"      , "U"      , "V"      , "W"      ,
    "X"      , "Y"      , "Z"      , "["      , R"(\\)"  , "]"      , "^"      , "_"      ,
    "`"      , "a"      , "b"      , "c"      , "d"      , "e"      , "f"      , "g"      ,
    "h"      , "i"      , "j"      , "k"      , "l"      , "m"      , "n"      , "o"      ,
    "p"      , "q"      , "r"      , "s"      , "t"      , "u"      , "v"      , "w"      ,
    "x"      , "y"      , "z"      , "{"      , "|"      , "}"      , "~"      , R"(\x7f)",
    R"(\x80)", R"(\x81)", R"(\x82)", R"(\x83)", R"(\x84)", R"(\x85)", R"(\x86)", R"(\x87)",
    R"(\x88)", R"(\x89)", R"(\x8a)", R"(\x8b)", R"(\x8c)", R"(\x8d)", R"(\x8e)", R"(\x8f)",
    R"(\x90)", R"(\x91)", R"(\x92)", R"(\x93)", R"(\x94)", R"(\x95)", R"(\x96)", R"(\x97)",
    R"(\x98)", R"(\x99)", R"(\x9a)", R"(\x9b)", R"(\x9c)", R"(\x9d)", R"(\x9e)", R"(\x9f)",
    R"(\xa0)", R"(\xa1)", R"(\xa2)", R"(\xa3)", R"(\xa4)", R"(\xa5)", R"(\xa6)", R"(\xa7)",
    R"(\xa8)", R"(\xa9)", R"(\xaa)", R"(\xab)", R"(\xac)", R"(\xad)", R"(\xae)", R"(\xaf)",
    R"(\xb0)", R"(\xb1)", R"(\xb2)", R"(\xb3)", R"(\xb4)", R"(\xb5)", R"(\xb6)", R"(\xb7)",
    R"(\xb8)", R"(\xb9)", R"(\xba)", R"(\xbb)", R"(\xbc)", R"(\xbd)", R"(\xbe)", R"(\xbf)",
    R"(\xc0)", R"(\xc1)", R"(\xc2)", R"(\xc3)", R"(\xc4)", R"(\xc5)", R"(\xc6)", R"(\xc7)",
    R"(\xc8)", R"(\xc9)", R"(\xca)", R"(\xcb)", R"(\xcc)", R"(\xcd)", R"(\xce)", R"(\xcf)",
    R"(\xd0)", R"(\xd1)", R"(\xd2)", R"(\xd3)", R"(\xd4)", R"(\xd5)", R"(\xd6)", R"(\xd7)",
    R"(\xd8)", R"(\xd9)", R"(\xda)", R"(\xdb)", R"(\xdc)", R"(\xdd)", R"(\xde)", R"(\xdf)",
    R"(\xe0)", R"(\xe1)", R"(\xe2)", R"(\xe3)", R"(\xe4)", R"(\xe5)", R"(\xe6)", R"(\xe7)",
    R"(\xe8)", R"(\xe9)", R"(\xea)", R"(\xeb)", R"(\xec)", R"(\xed)", R"(\xee)", R"(\xef)",
    R"(\xf0)", R"(\xf1)", R"(\xf2)", R"(\xf3)", R"(\xf4)", R"(\xf5)", R"(\xf6)", R"(\xf7)",
    R"(\xf8)", R"(\xf9)", R"(\xfa)", R"(\xfb)", R"(\xfc)", R"(\xfd)", R"(\xfe)", R"(\xff)",
};

namespace utils {
std::string repr(const std::string &str) {
    return repr(str.data(), str.size());
}

std::string repr(const char *buf, size_t size) {
    uint8_t cc;
    std::string ret;

    /* reserve space for the result, sufficient for most cases */
    ret.reserve(size * 2);
    ret.push_back('"');

    /* map every character */
    for (size_t i = 0; i < size; i++) {
        cc = buf[i];
        ret += HexTable[cc];
    }

    /* shrink to fit the result */
    ret.push_back('"');
    ret.shrink_to_fit();
    return ret;
}
}
