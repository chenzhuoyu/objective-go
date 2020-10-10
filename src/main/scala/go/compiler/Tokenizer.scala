package go.compiler

import java.lang.{Double => JDouble}
import java.nio.charset.StandardCharsets

import go.utils.{Location, OpTree}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class Tokenizer(src: String, fname: String) extends Location.Carrier {
    trait State {
        def col: Int
        def row: Int
        def pos: Int
    }

    private[this] case class TState(
        var col: Int = 0,
        var row: Int = 0,
        var pos: Int = 0
    ) extends State

    private[this] var s  : TState = TState()
    private[this] var st : TState = TState()

    private[this] val buf: Seq[Int] = {
        src.codePoints().iterator().asScala.toIndexedSeq.map(_.intValue) match {
            case v @ _ :+ '\n' => v
            case v             => v :+ '\n'
        }
    }

    private[this] final val Keywords = Map(
        "async"      -> Token.Async,
        "break"      -> Token.Break,
        "case"       -> Token.Case,
        "catch"      -> Token.Catch,
        "chan"       -> Token.Chan,
        "class"      -> Token.Class,
        "const"      -> Token.Const,
        "continue"   -> Token.Continue,
        "default"    -> Token.Default,
        "defer"      -> Token.Defer,
        "else"       -> Token.Else,
        "extends"    -> Token.Extends,
        "false"      -> Token.False,
        "finally"    -> Token.Finally,
        "for"        -> Token.For,
        "func"       -> Token.Func,
        "if"         -> Token.If,
        "implements" -> Token.Implements,
        "import"     -> Token.Import,
        "in"         -> Token.In,
        "interface"  -> Token.Interface,
        "map"        -> Token.Map,
        "new"        -> Token.New,
        "nil"        -> Token.Nil,
        "package"    -> Token.Package,
        "return"     -> Token.Return,
        "select"     -> Token.Select,
        "struct"     -> Token.Struct,
        "switch"     -> Token.Switch,
        "template"   -> Token.Template,
        "throw"      -> Token.Throw,
        "true"       -> Token.True,
        "try"        -> Token.Try,
        "type"       -> Token.Type,
        "var"        -> Token.Var,
    )

    private[this] final val Operators = OpTree(
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
        "->"  -> Token.`->`,
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
    )

    private[this] final val StdEscape = Map(
        ('"'  : Int) -> Array[Byte]('\"'),
        ('a'  : Int) -> Array[Byte](0x07),
        ('b'  : Int) -> Array[Byte]('\b'),
        ('f'  : Int) -> Array[Byte]('\f'),
        ('n'  : Int) -> Array[Byte]('\n'),
        ('r'  : Int) -> Array[Byte]('\r'),
        ('t'  : Int) -> Array[Byte]('\t'),
        ('v'  : Int) -> Array[Byte](0x0b),
        ('\'' : Int) -> Array[Byte]('\''),
        ('\\' : Int) -> Array[Byte]('\\'),
    )

    override def col  : Int    = st.col
    override def row  : Int    = st.row
    override val file : String = fname

    def eof               : Boolean = s.pos >= buf.length
    def next              : Token   = parse()
    def state             : State   = s.copy()
    def state_=(v: State) : Unit    = s = TState(v.col, v.row, v.pos)

    /** Implicit Conversions **/

    private[this] implicit class SeqIntOps(v: Seq[Int]) {
        def str   : String      = v.map(Character.toString).mkString
        def bytes : Array[Byte] = v.flatMap(Character.toString(_).getBytes()).toArray
    }

    /** Character Classes **/

    private[this] def isBin(ch: Int): Boolean = ch == '0' || ch == '1'
    private[this] def isOct(ch: Int): Boolean = '0' <= ch && ch <= '7'
    private[this] def isHex(ch: Int): Boolean = '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F'

    /** Character Readers **/

    private[this] def thisChar: Int = buf(s.pos)
    private[this] def peekChar: Int = if (eof) 0 else thisChar

    private[this] def readChar: Int = {
        st = s.copy()
        nextChar
    }

    private[this] def nextChar: Int = if (eof) 0 else {
        val cp = s.pos
        val ch = buf(cp)

        /* check for new line */
        if (ch != '\n') {
            s.col += 1
        } else {
            s.col = 0
            s.row += 1
        }

        /* advance read pointer */
        s.pos += 1
        ch
    }

    /** Component Skippers **/

    private[this] def skipQuote(): Boolean = {
        { nextChar; peekChar } == '`' &&
        { nextChar; true }
    }

    @tailrec
    private[this] def skipSpace(ch: Int): Int = ch match {
        case c if Character.isWhitespace(c) => skipSpace(readChar)
        case c                              => c
    }

    @tailrec
    private[this] def skipBlanks(): Int = {
        skipComments() match {
            case None    => skipBlanks()
            case Some(c) => c
        }
    }

    private[this] def skipComments(): Option[Int] = {
        skipSpace(readChar) match {
            case '/' => skipCComments()
            case '#' => skipLineComments()
            case ret => Some(ret)
        }
    }

    private[this] def skipCComments(): Option[Int] = {
        peekChar match {
            case '/' => skipLineComments()
            case '*' => skipBlockComments()
            case _   => Some('/')
        }
    }

    private[this] def skipLineComments(): Option[Int] = {
        readLine()
        None
    }

    private[this] def skipBlockComments(): Option[Int] = {
        readFast(s.pos, buf.indexOfSlice("*/", s.pos), 2)
        None
    }

    /** Component Readers **/

    private[this] def readLine(): Seq[Int] = {
        val pos = s.pos
        val end = buf.indexOf('\n', pos)

        /* adjust state, and slice directly from source */
        s.pos = end
        s.col += end - pos
        buf.slice(pos, end)
    }

    private[this] def readFast(pos: Int, end: Int, tail: Int): Seq[Int] = {
        val ret = buf.slice(pos, end)
        val nnl = ret.count(_ == '\n')

        /* adjust column based on new-line count */
        if (nnl > 0) {
            s.col = -ret.lastIndexOf('\n') - 1
        }

        /* adjust tokenizer state */
        s.pos = end + tail
        s.row += nnl
        s.col += end + tail - pos
        ret
    }

    private[this] def readUntil(quote: Int, delim: Option[Int] = None): (Int, Array[Byte]) = {
        val pos = s.pos
        val idx = buf.indexOf(quote, pos)

        /* also consider delimiter if needed */
        val end = delim
            .map  (buf.indexOf(_, pos))
            .map  (Seq(idx, _))
            .fold (idx) { _.filterNot(_ < 0).min }

        /* it is faster to slice directly from source */
        if (end < 0) {
            throw SyntaxError("unexpected EOF")
        } else {
            (buf(end), readFast(pos, end, 1).bytes)
        }
    }

    @tailrec
    private[this] def readDigits(name: String, pred: Int => Boolean, n: Int, str: Seq[Int] = Seq()): String = {
        if (n == 0) {
            str.str
        } else if (!eof && pred(thisChar)) {
            readDigits(name, pred, n - 1, str :+ nextChar)
        } else {
            throw SyntaxError(s"too few $name digits")
        }
    }

    private[this] def readEscape(ch: Int): Array[Byte] = ch match {
        case 0             => throw SyntaxError("unexpected EOF")
        case 'x'           => readEscapeHex()
        case 'u'           => readEscapeUni(4)
        case 'U'           => readEscapeUni(8)
        case StdEscape(x)  => x
        case c if isOct(c) => readEscapeOct(c)
        case c             => throw SyntaxError(s"invalid escape character '$c'")
    }

    private[this] def readEscapeHex(): Array[Byte] = {
        Array(Integer.parseUnsignedInt(readDigits("hexadecimal", isHex, 2), 16).toByte)
    }

    private[this] def readEscapeOct(ch: Int): Array[Byte] = {
        val str = readDigits("octal", isOct, 2)
        val ret = Integer.parseUnsignedInt(Character.toString(ch) + str, 8)

        /* check for octal range */
        if (ret <= 256) {
            Array(ret.toByte)
        } else {
            throw SyntaxError(s"octal escape value > 255: $ret")
        }
    }

    private[this] def readEscapeUni(size: Int): Array[Byte] = {
        val str = readDigits("hexadecimal", isHex, size)
        val chr = Integer.parseUnsignedInt(str, 16)

        /* check for unicode range */
        if (chr > Character.MAX_CODE_POINT) {
            throw SyntaxError("invalid Unicode code point: 0x%08x".format(chr))
        } else if (Character.MIN_SURROGATE <= chr && chr <= Character.MAX_SURROGATE) {
            throw SyntaxError("surrogate half: 0x%04x".format(chr))
        } else {
            Character.toString(chr).getBytes(StandardCharsets.UTF_8)
        }
    }

    /** Component Parsers **/

    private[this] def parse(): Token = {
        skipBlanks() match {
            case 0                                          => Token.End()
            case '\n'                                       => Token.LF()
            case '`'                                        => parseRaw()
            case '\''                                       => parseRune()
            case '"'                                        => parseString()
            case '.'                                        => parseDecimal()
            case '_'                                        => parseIdentifier('_')
            case c @ Operators(_)                           => parseOperator(c)
            case c if Character.isDigit(c)                  => parseNumber(c)
            case c if Character.isUnicodeIdentifierStart(c) => parseIdentifier(c)
            case c                                          => throw SyntaxError(s"invalid character: '$c'")
        }
    }

    private[this] def parseRaw(): Token = {
        if (peekChar != '`') {
            parseSingleRaw()
        } else if (skipQuote()) {
            parseTripleRaw(mutable.ArrayBuilder.make)
        } else {
            Token.String(Array())
        }
    }

    private[this] def parseSingleRaw(): Token = {
        val (_, ret) = readUntil('`')
        Token.String(ret)
    }

    @tailrec
    private[this] def parseTripleRaw(ret: mutable.ArrayBuilder[Byte]): Token = {
        ret.addAll(readUntil('`')._2); {
            if (peekChar != '`') {
                parseTripleRaw(ret.addOne('`'))
            } else if (skipQuote()) {
                Token.String(ret.result())
            } else {
                parseTripleRaw(ret.addOne('`').addOne('`'))
            }
        }
    }

    private[this] def parseRune(): Token = {
        parseRuneEnd {
            nextChar match {
                case '\'' => throw SyntaxError("empty rune")
                case '\\' => new String(readEscape(nextChar)).codePointAt(0)
                case c    => c.toInt
            }
        }
    }

    private[this] def parseRuneEnd(ch: Int): Token = {
        if (nextChar == '\'') {
            Token.Rune(ch)
        } else {
            throw SyntaxError("too many characters")
        }
    }

    private[this] def parseString(): Token = {
        val ret = mutable.ArrayBuilder.make[Byte]
        var buf = readUntil('"', Some('\\'))

        /* scan until end of quote */
        while (buf._1 != '"') {
            ret ++= buf._2
            ret ++= readEscape(nextChar)
            buf   = readUntil('"', Some('\\'))
        }

        /* build the token */
        Token.String(ret.addAll(buf._2).result())
    }

    private[this] def parseDecimal(): Token = {
        if (eof || !Character.isDigit(thisChar)) {
            parseOperator('.')
        } else {
            parseNumberFloat(mutable.ArrayBuilder.make[Int].addAll(Seq[Int]('0', '.')))
        }
    }

    private[this] def parseNumber(first: Int): Token = {
        var nch = peekChar
        val iab = mutable.ArrayBuilder.make[Int].addOne(first)

        /* special case of leading zero */
        if (first == '0') {
            nch match {
                case 0         => return Token.Integer(0)
                case 'b' | 'B' => return parseNumberBin()
                case 'o' | 'O' => return parseNumberOct()
                case 'x' | 'X' => return parseNumberHex()
                case _         =>
            }
        }

        /* scan each digit */
        while (Character.isDigit(nch)) {
            iab += nextChar
            nch  = peekChar
        }

        /* build the array */
        val ret = iab.result()
        val buf = ret.toSeq

        /* check for decimal point, complex numbers and scientific notation */
        nch match {
            case '.'                    => parseNumberFloat(iab)
            case 'i'                    => parseNumberComplex(iab)
            case 'e' | 'E'              => parseNumberScientific(iab)
            case _ if first != '0'      => Token.Integer(BigInt(buf.str))
            case _ if buf.forall(isOct) => Token.Integer(BigInt(buf.str, 8))
            case c                      => throw SyntaxError(s"invalid octal digit: '$c'")
        }
    }

    private[this] def parseNumberHex(): Token = {
        nextChar
        parseNumberWithCharset("hex", 16, isHex)
    }

    private[this] def parseNumberOct(): Token = {
        nextChar
        parseNumberWithCharset("octal", 8, isOct)
    }

    private[this] def parseNumberBin(): Token = {
        nextChar
        parseNumberWithCharset("binary", 2, isBin)
    }

    private[this] def parseNumberFloat(ret: mutable.ArrayBuilder[Int]): Token = {
        val rem = ret.addOne(nextChar)
        var nch = peekChar

        /* scan every digit */
        while (Character.isDigit(nch)) {
            rem += nextChar
            nch  = peekChar
        }

        /* check for complex number or scientific notation */
        nch match {
            case 'i'       => parseNumberComplex(rem)
            case 'e' | 'E' => parseNumberScientific(rem)
            case _         => Token.Float(JDouble.parseDouble(rem.result().toSeq.str))
        }
    }

    private[this] def parseNumberComplex(ret: mutable.ArrayBuilder[Int]): Token = {
        nextChar
        Token.Complex(JDouble.parseDouble(ret.result().toSeq.str))
    }

    private[this] def parseNumberScientific(ret: mutable.ArrayBuilder[Int]): Token = {
        var ok  = false
        val rem = ret.addOne(nextChar)
        var nch = peekChar

        /* check for exponent sign */
        if (nch == '+' || nch == '-') {
            rem += nextChar
            nch  = peekChar
        }

        /* scan every digit */
        while (Character.isDigit(nch)) {
            ok   = true
            rem += nextChar
            nch  = peekChar
        }

        /* check for result */
        if (!ok) {
            throw SyntaxError("invalid float literal")
        } else if (nch == 'i') {
            parseNumberComplex(ret)
        } else {
            Token.Float(JDouble.parseDouble(rem.result().toSeq.str))
        }
    }

    private[this] def parseNumberWithCharset(name: String, base: Int, pred: Int => Boolean): Token = {
        var nch = peekChar
        val ret = mutable.ArrayBuilder.make[Int]

        /* scan every digit */
        while (pred(nch)) {
            ret += nextChar
            nch  = peekChar
        }

        /* check for result */
        if (!isHex(nch) && ret.knownSize != 0) {
            Token.Integer(BigInt(ret.result().toSeq.str, base))
        } else {
            throw SyntaxError(s"invalid $name digit")
        }
    }

    private[this] def parseOperator(first: Int): Token = {
        var pst = s.copy()
        var nch = peekChar
        var opv = Operators(first)
        var ret = opv.value

        /* traverse the trie tree */
        while (opv.contains(nch)) {
            opv = opv(nextChar)
            nch = peekChar

            /* commit the current state for leaf nodes */
            opv.value.foreach { v =>
                ret = Some(v)
                pst = s.copy()
            }
        }

        /* build the token */
        ret.fold(throw SyntaxError("invalid operator: " + first)) { v =>
            s = pst.copy()
            Token.Operator(v)
        }
    }

    private[this] def parseIdentifier(first: Int): Token = {
        var nch = peekChar
        val ret = mutable.ArrayBuilder.make[Int].addOne(first)

        /* scan until no more identifier characters */
        while (nch != 0 && Character.isUnicodeIdentifierPart(nch)) {
            ret += nextChar
            nch  = peekChar
        }

        /* check for keywords */
        ret.result().toSeq.str match {
            case Keywords(x) => Token.Keyword(x)
            case s           => Token.Name(s)
        }
    }
}
