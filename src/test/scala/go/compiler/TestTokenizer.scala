package go.compiler

import org.junit.Assert._
import org.junit.Test
import org.junit.internal.ExactComparisonCriteria

class TestTokenizer {
    import java.util.Objects

    private def repr(v: Any): String = {
        if (v == null) {
            "null"
        } else if (!v.getClass.isArray) {
            v.toString
        } else {
            (0 until java.lang.reflect.Array.getLength(v))
                .map      (java.lang.reflect.Array.get(v, _))
                .map      (Objects.toString)
                .mkString ("[", ", ", "]")
        }
    }

    private def isArray(v: Any): Boolean = {
        v != null && v.getClass.isArray
    }

    private def verify[V](src: String, expect: V)(pred: PartialFunction[Token, V]): Unit = {
        val tk = Tokenizer(src, "<test>")
        val tv = tk.next

        pred.lift(tv) match {
            case None    => assertTrue("wrong token class: " + tv, false)
            case Some(v) => if (!isArray(v)) {
                assertEquals("wrong token value: " + v, expect, v)
            } else {
                new ExactComparisonCriteria().arrayEquals("wrong token value: " + v, expect, v)
            }
        }

        val nt = tk.next
        assertTrue("should be EOF, but got: " + nt, nt.isInstanceOf[Token.End])
        println(s"* Test :: '$src' -> ${repr(expect)} @ ${tv.file}:${tv.row}:${tv.col}")
    }

    @Test
    def testComments(): Unit = {
        verify("# test comment", null) { case Token.End() => null }
        verify("// test comment", null) { case Token.End() => null }
        verify("/* test comment */", null) { case Token.End() => null }
        verify("/* test\ncomment */", null) { case Token.End() => null }
    }

    @Test
    def testNames(): Unit = {
        for (name <- Seq(
            "a",
            "_x9",
            "mixedCaseVariable",
            "αβ"
        )) verify(name, name) { case Token.Name(v) => v }
    }

    @Test
    def testKeywords(): Unit = {
        for ((name, exp) <- Seq(
            "break"     -> Token.Break,
            "case"      -> Token.Case,
            "catch"     -> Token.Catch,
            "chan"      -> Token.Chan,
            "class"     -> Token.Class,
            "const"     -> Token.Const,
            "continue"  -> Token.Continue,
            "default"   -> Token.Default,
            "defer"     -> Token.Defer,
            "else"      -> Token.Else,
            "enum"      -> Token.Enum,
            "extends"   -> Token.Extends,
            "finally"   -> Token.Finally,
            "for"       -> Token.For,
            "func"      -> Token.Func,
            "go"        -> Token.Go,
            "if"        -> Token.If,
            "import"    -> Token.Import,
            "in"        -> Token.In,
            "interface" -> Token.Interface,
            "new"       -> Token.New,
            "override"  -> Token.Override,
            "package"   -> Token.Package,
            "return"    -> Token.Return,
            "select"    -> Token.Select,
            "struct"    -> Token.Struct,
            "super"     -> Token.Super,
            "switch"    -> Token.Switch,
            "throw"     -> Token.Throw,
            "try"       -> Token.Try,
            "type"      -> Token.Type,
            "with"      -> Token.With,
            "var"       -> Token.Var,
        )) verify[Token.KeywordType](name, exp) { case Token.Keyword(v) => v }
    }

    @Test
    def testOperators(): Unit = {
        for ((name, exp) <- Seq(
            "+"   -> Token.`+`,
            "-"   -> Token.`-`,
            "*"   -> Token.`*`,
            "/"   -> Token.`/`,
            "%"   -> Token.`%`,
            "&"   -> Token.`&`,
            "|"   -> Token.`|`,
            "^"   -> Token.`^`,
            "~"   -> Token.`~`,
            "<<"  -> Token.`<<`,
            ">>"  -> Token.`>>`,
            "+="  -> Token.`+=`,
            "-="  -> Token.`-=`,
            "*="  -> Token.`*=`,
            "/="  -> Token.`/=`,
            "%="  -> Token.`%=`,
            "&="  -> Token.`&=`,
            "|="  -> Token.`|=`,
            "^="  -> Token.`^=`,
            "~="  -> Token.`~=`,
            "<<=" -> Token.`<<=`,
            ">>=" -> Token.`>>=`,
            "&&"  -> Token.`&&`,
            "||"  -> Token.`||`,
            "!"   -> Token.`!`,
            "=>"  -> Token.`=>`,
            "<-"  -> Token.`<-`,
            "++"  -> Token.`++`,
            "--"  -> Token.`--`,
            "=="  -> Token.`==`,
            "<"   -> Token.`<`,
            ">"   -> Token.`>`,
            "!="  -> Token.`!=`,
            "<="  -> Token.`<=`,
            ">="  -> Token.`>=`,
            "="   -> Token.`=`,
            ":="  -> Token.`:=`,
            "..." -> Token.`...`,
            "("   -> Token.`(`,
            ")"   -> Token.`)`,
            "["   -> Token.`[`,
            "]"   -> Token.`]`,
            "{"   -> Token.`{`,
            "}"   -> Token.`}`,
            ","   -> Token.`,`,
            "."   -> Token.`.`,
            ";"   -> Token.`;`,
            ":"   -> Token.`:`,
            "?"   -> Token.`?`,
        )) verify[Token.OperatorType](name, exp) { case Token.Operator(v) => v }
    }

    @Test
    def testIntegers(): Unit = {
        for ((name, exp) <- Seq(
            "42"                                      -> BigInt("42"),
            "0600"                                    -> BigInt("600", 8),
            "0o777"                                   -> BigInt("777", 8),
            "0xBadFace"                               -> BigInt("BadFace", 16),
            "0b110100101"                             -> BigInt("110100101", 2),
            "170141183460469231731687303715884105727" -> BigInt("170141183460469231731687303715884105727"),
        )) verify(name, exp) { case Token.Integer(v) => v }
    }

    @Test
    def testFloats(): Unit = {
        for ((name, exp) <- Seq(
            "0."          -> .0,
            ".25"         -> .25,
            "72.40"       -> 72.40,
            "2.71828"     -> 2.71828,
            "072.40"      -> 072.40,    // == 72.40
            "1.e+0"       -> 1.0e+0,
            "6.67428e-11" -> 6.67428e-11,
            "1E6"         -> 1E6,
            ".12345E+5"   -> .12345E+5,
        )) verify(name, exp) { case Token.Float(v) => v }
    }

    @Test
    def testComplexes(): Unit = {
        for ((name, exp) <- Seq(
            "0.i"          -> .0,
            ".25i"         -> .25,
            "72.40i"       -> 72.40,
            "2.71828i"     -> 2.71828,
            "072.40i"      -> 072.40,    // == 72.40
            "1.e+0i"       -> 1.0e+0,
            "6.67428e-11i" -> 6.67428e-11,
            "1E6i"         -> 1E6,
            ".12345E+5i"   -> .12345E+5,
        )) verify(name, exp) { case Token.Complex(v) => v }
    }

    @Test
    def testStrings(): Unit = {
        for ((name, exp) <- Seq(
            "`abc`"                                             -> "abc".getBytes,
            "\"\\n\""                                           -> "\n".getBytes,
            "\"\\\"\""                                          -> "\"".getBytes,
            "\"\\033\""                                         -> "\u001b".getBytes,
            "\"Hello, world!\\n\""                              -> "Hello, world!\n".getBytes,
            "\"日本語\""                                         -> "日本語".getBytes,
            "\"\\u65e5本\\U00008a9e\""                          -> "\u65e5本\u8a9e".getBytes,
            "\"\\xff\\u00FF\""                                  -> (Array[Byte](0xff.toByte) ++ "\u00FF".getBytes()),
            "`日本語`"                                           -> "日本語".getBytes,
            "\"\\u65e5\\u672c\\u8a9e\""                         -> "日本語".getBytes,
            "\"\\U000065e5\\U0000672c\\U00008a9e\""             -> "日本語".getBytes,
            "\"\\xe6\\x97\\xa5\\xe6\\x9c\\xac\\xe8\\xaa\\x9e\"" -> "日本語".getBytes,
            "```SELECT `row` FROM `table` WHERE `id` = 1```"    -> "SELECT `row` FROM `table` WHERE `id` = 1".getBytes,
        )) verify(name, exp) { case Token.String(v) => v }
    }

    @Test
    def testMultilineString(): Unit = {
        verify("`asdf\nqwer`", "asdf\nqwer".getBytes) {
            case Token.String(v) => v
        }
    }

    @Test
    def testStringSurrogateHalf(): Unit = {
        try {
            verify("\"\\ud800\"", Array[Byte]()) { case _ => Array[Byte]() }
            throw new AssertionError("no exception thrown")
        } catch {
            case SyntaxError(msg) => assertEquals("surrogate half: 0xd800", msg)
        }
    }

    @Test
    def testStringInvalidUnicodeCodePoint(): Unit = {
        try {
            verify("\"\\U00110000\"", Array[Byte]()) { case _ => Array[Byte]() }
            throw new AssertionError("no exception thrown")
        } catch {
            case SyntaxError(msg) => assertEquals("invalid Unicode code point: 0x00110000", msg)
        }
    }
}
