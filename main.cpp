#include "utils/format.h"
#include "tokenizer/token.h"

int main() {
    fmt::printf("%s\n", *Token::NoSplit);
    return 0;
}
