package go.compiler

import go.compiler.Token.OperatorType
import go.utils.Location.{Carrier, Snapshot}
import go.utils.MonoBuffer

import scala.annotation.tailrec

object Parser {
    private final val UnaryOps: Set[OperatorType] = Set(
        Token.`+`,
        Token.`-`,
        Token.`!`,
        Token.`~`,
        Token.`<-`,
    )

    private final val BinaryOps: Seq[Set[OperatorType]] = Seq(
        Set(Token.`||`),
        Set(Token.`&&`),
        Set(Token.`==`, Token.`!=`, Token.`<`, Token.`>`, Token.`<=`, Token.`>=`),
        Set(Token.`+` , Token.`-` , Token.`|`, Token.`^`),
        Set(Token.`*` , Token.`/` , Token.`%`, Token.`&`, Token.`<<`, Token.`>>`),
    )

    private final val PrefixOps: Map[OperatorType, OperatorType] = Map(
        Token.`++` -> Token.Prefix(Token.`++`),
        Token.`--` -> Token.Prefix(Token.`--`),
    )

    private final val PostfixOps: Map[OperatorType, OperatorType] = Map(
        Token.`++` -> Token.Postfix(Token.`++`),
        Token.`--` -> Token.Postfix(Token.`--`),
    )

    private final object IsEnd {
        def unapply(v: Token): Option[Unit] = v match {
            case Token.Nil()                   => Some()
            case Token.Name(_)                 => Some()
            case Token.Bool(_)                 => Some()
            case Token.Rune(_)                 => Some()
            case Token.Float(_)                => Some()
            case Token.String(_)               => Some()
            case Token.Complex(_)              => Some()
            case Token.Integer(_)              => Some()
            case Token.Keyword(Token.Break)    => Some()
            case Token.Keyword(Token.Return)   => Some()
            case Token.Keyword(Token.Continue) => Some()
            case Token.Operator(Token.`)`)     => Some()
            case Token.Operator(Token.`]`)     => Some()
            case Token.Operator(Token.`}`)     => Some()
            case Token.Operator(Token.`>`)     => Some()
            case Token.Operator(Token.`++`)    => Some()
            case Token.Operator(Token.`--`)    => Some()
            case _                             => None
        }
    }

    private final object IsPrefix {
        def unapply(v: Token): Option[Unit] = v match {
            case Token.Operator(Token.Prefix(_)) => Some()
            case _                               => None
        }
    }

    private final object AsPostfix {
        def unapply(v: Token): Option[OperatorType] = v match {
            case Token.Operator(PostfixOps(v)) => Some(v)
            case _                             => None
        }
    }
}

case class Parser(tk: Tokenizer) extends Carrier {
    import Parser._

    override def col  : Int    = tk.col
    override def row  : Int    = tk.row
    override val file : String = tk.file

    private[this] var expr  : Int               = 0
    private[this] var last  : Option[Token]     = None
    private[this] val token : MonoBuffer[Token] = MonoBuffer(read)

    /** Parser Interface **/

    lazy val result: AST.Package = {
        throw new NotImplementedError("not implemented: Parser.result")
    }

    /** Tokenizer Management Helpers **/

    private[this] def read: Token = {
        tk.next match {
            case Token.LF() => readLF
            case p          => last = Some(p); p
        }
    }

    private[this] def readLF: Token = {
        last match {
            case Some(IsEnd(_)) => last = None; Token.Operator(Token.`;`)
            case _              => skipLF
        }
    }

    @tailrec
    private[this] def skipLF: Token = {
        tk.next match {
            case Token.LF() => skipLF
            case p          => last = Some(p); p
        }
    }

    @inline private[this] final def next: Token = token.next
    @inline private[this] final def peek: Token = token.peek

    @inline private[this] final def drop[T](f: => T)          : T = { next; f }
    @inline private[this] final def save[T](f: Snapshot => T) : T = f(Snapshot())

    /** Parsing Helpers **/

    private[this] def eof[T](ret: T): T = {
        next match {
            case Token.End() => ret
            case t           => throw SyntaxError(s"EOF expected, found $t")
        }
    }

    private[this] def ends[T](ret: T, op: Token.OperatorType): T = {
        next match {
            case Token.Operator(v) if v == op => ret
            case _                            => throw SyntaxError(s"operator '$op' expected")
        }
    }

    private[this] def nested[T](value: => T): T = {
        try {
            expr += 1
            value
        } finally {
            expr -= 1
        }
    }

    private[this] def semicolon(): Unit = {
        peek match {
            case Token.Operator(Token.`)`) =>
            case Token.Operator(Token.`}`) =>
            case Token.Operator(Token.`;`) => next
            case _                         => throw SyntaxError("';' or new line expected")
        }
    }

    private[this] def isSeqEnds(op: Token.OperatorType): Boolean = {
        peek match {
            case Token.Operator(t) => t == op
            case _                 => false
        }
    }

    private[this] def sequence[T](op: Token.OperatorType)(next: => T): Seq[T] = {
        peek match {
            case Token.Operator(t) if t == op => Seq()
            case _                            => sequence(op, Seq(next))(next)
        }
    }

    @tailrec
    private[this] def sequence[T](op: Token.OperatorType, ret: Seq[T])(next: => T): Seq[T] = {
        next match {
            case Token.Operator(t)         if t == op       => ret
            case Token.Operator(Token.`,`) if isSeqEnds(op) => drop(ret)
            case Token.Operator(Token.`,`)                  => sequence(op, ret :+ next)(next)
            case _                                          => throw SyntaxError(s"'$op' or ',' expected")
        }
    }

    /** Type Parsing **/

    private[this] def parseType(): AST.Type = {
        _ // TODO: parse type
    }

    private[this] def parseTypeName(): AST.TypeName = {
        _ // TODO: pkg.Type
    }

    /** Generic Parsing **/

    private[this] def parseGenericImpl(): Seq[AST.Type] = {
        sequence(Token.`>`)(parseType())
    }

    private[this] def parseGenericSpec(): Seq[AST.GenericSpec] = {
        sequence(Token.`]`) {
            next match {
                case Token.Name(v)             => parseGenericBounds(AST.Invariant, AST.Name(v))
                case Token.Operator(Token.`+`) => parseGenericName(AST.Covariant)
                case Token.Operator(Token.`-`) => parseGenericName(AST.Contravariant)
                case _                         => throw SyntaxError("'+', '-' or identifier expected")
            }
        }
    }

    private[this] def parseGenericName(va: AST.Variance): AST.GenericSpec = {
        next match {
            case Token.Name(v) => parseGenericBounds(va, AST.Name(v))
            case _             => throw SyntaxError("identifier expected")
        }
    }

    private[this] def parseGenericBounds(va: AST.Variance, name: AST.Name): AST.GenericSpec = {
        peek match {
            case Token.Operator(Token.`>=`) => drop(parseGenericRanges(va, name, parseTypeName()))
            case Token.Operator(Token.`<=`) => drop(AST.GenericSpec(name, va, None, Some(parseTypeName()))(name))
            case _                          => AST.GenericSpec(name, va, None, None)(name)
        }
    }

    private[this] def parseGenericRanges(va: AST.Variance, name: AST.Name, lower: AST.TypeName): AST.GenericSpec = {
        peek match {
            case Token.Operator(Token.`<=`) => drop(AST.GenericSpec(name, va, Some(lower), Some(parseTypeName()))(name))
            case _                          => AST.GenericSpec(name, va, Some(lower), None)(name)
        }
    }

    /** Primary Factor Parsing **/

    private[this] def parsePrimary(): AST.Primary = {
        save { implicit p =>
            AST.Primary(parsePrimaryBase(), parsePrimaryMods(Seq()))
        }
    }

    private[this] def parsePrimaryBase(): AST.Operand = {
        peek match {
            case Token.Nil()               => AST.Nil()(next)
            case Token.Name(v)             => AST.Name(v)(next)
            case Token.Bool(v)             => AST.BoolLit(v)(next)
            case Token.Rune(v)             => AST.RuneLit(v)(next)
            case Token.Float(v)            => AST.FloatLit(v)(next)
            case Token.String(v)           => AST.StringLit(v)(next)
            case Token.Integer(v)          => AST.IntegerLit(v)(next)
            case Token.Complex(v)          => AST.ComplexLit(v)(next)
            case Token.Keyword(Token.New)  => drop(parsePrimaryConstruct(parseTypeName()))
            case Token.Operator(Token.`(`) => drop(ends(parseExpr(), Token.`)`))
            case t                         => throw SyntaxError("operands expected, got " + t)
        }
    }

    @tailrec
    private[this] def parsePrimaryMods(ret: Seq[AST.Modifier]): Seq[AST.Modifier] = {
        peek match {
            case Token.Operator(Token.`(`) => parsePrimaryMods(ret :+ drop(parseInvoke()))
            case Token.Operator(Token.`<`) => parsePrimaryMods(ret :+ drop(parseTypedInvoke()))
            case Token.Operator(Token.`[`) => parsePrimaryMods(ret :+ drop(parseIndexOrSlice()))
            case Token.Operator(Token.`.`) => parsePrimaryMods(ret :+ drop(parseMethodSelectorOrConversion()))
            case _                         => ret
        }
    }

    private[this] def parsePrimaryConstruct(vt: AST.TypeName): AST.Construct = {
        save { implicit p => peek match {
            case Token.Operator(Token.`(`) => AST.Construct(vt, Some(drop(parseInvoke())))
            case Token.Operator(Token.`<`) => AST.Construct(vt, Some(drop(parseTypedInvoke())))
            case _                         => AST.Construct(vt, None)
        }}
    }

    /** Primary Modifier Parsing **/

    private[this] def parseSLen(): Option[AST.Expression] = {
        peek match {
            case Token.Operator(Token.`]`) => None
            case Token.Operator(Token.`:`) => None
            case _                         => Some(parseExpr())
        }
    }

    private[this] def parseSCap(len: Option[_]): Option[AST.Expression] = {
        (peek, len) match {
            case (Token.Operator(Token.`:`), None) => throw SyntaxError("length expression is required")
            case (Token.Operator(Token.`:`), _   ) => Some(drop(nested(parseExpr())))
            case _                                 => None
        }
    }

    private[this] def parseSlice(pos: Option[AST.Expression])(implicit p: Snapshot): AST.Slice = {
        val len = parseSLen()
        val cap = parseSCap(len)
        ends(AST.Slice(pos, len, cap), Token.`]`)
    }

    private[this] def parseInvoke(gs: Seq[AST.Type] = Seq()): AST.Invoke = {
        save { implicit p => peek match {
            case Token.Operator(Token.`)`) => AST.Invoke(false, gs, Seq())
            case _                         => parseInvokeArgs(gs, Seq(parseExpr()))
        }}
    }

    @tailrec
    private[this] def parseInvokeArgs(gs: Seq[AST.Type], args: Seq[AST.Expression])(implicit p: Snapshot): AST.Invoke = {
        next match {
            case Token.Operator(Token.`)`)                         => AST.Invoke(false, gs, args)
            case Token.Operator(Token.`,`) if isSeqEnds(Token.`)`) => drop(AST.Invoke(false, gs, args))
            case Token.Operator(Token.`,`)                         => parseInvokeArgs(gs, args :+ parseExpr())
            case Token.Operator(Token.`...`)                       => ends(AST.Invoke(true, gs, args), Token.`)`)
            case _                                                 => throw SyntaxError("')', ',' or '...' expected")
        }
    }

    private[this] def parseTypedInvoke(): AST.Invoke = {
        parseBeginInvoke(parseGenericImpl())
    }

    private[this] def parseBeginInvoke(gs: Seq[AST.Type]): AST.Invoke = {
        next match {
            case Token.Operator(Token.`(`) => drop(parseInvoke(gs))
            case _                         => throw SyntaxError("'(' expected")
        }
    }

    private[this] def parseIndexOrSlice(): AST.Modifier = {
        save { implicit p => peek match {
            case Token.Operator(Token.`:`) => drop(parseSlice(None))
            case _                         => parseIndexOrSlice(nested(parseExpr()))
        }}
    }

    private[this] def parseIndexOrSlice(v: AST.Expression)(implicit p: Snapshot): AST.Modifier = {
        next match {
            case Token.Operator(Token.`:`) => parseSlice(Some(v))
            case Token.Operator(Token.`]`) => AST.Index(v)
            case _                         => throw SyntaxError("':' or ']' expected")
        }
    }

    private[this] def parseMethodOrSelector(name: AST.Name): AST.Modifier = {
        peek match {
            case p @ Token.Operator(Token.`(`) => drop(AST.Method(name, parseInvoke())(p))
            case p @ Token.Operator(Token.`<`) => drop(AST.Method(name, parseTypedInvoke())(p))
            case _                             => AST.Selector(name)(name)
        }
    }

    private[this] def parseMethodSelectorOrConversion(): AST.Modifier = {
        peek match {
            case Token.Name(v)             => drop(parseMethodOrSelector(AST.Name(v)))
            case Token.Operator(Token.`(`) => drop(ends(AST.Conversion(parseType()), Token.`)`))
            case _                         => throw SyntaxError("identifier or '(' expected")
        }
    }

    /** Expression Parsing **/

    private[this] def parseExpr(): AST.Expression = {
        trimExpressionTree(parseExpr(0))
    }

    private[this] def parseExpr(prec: Int): AST.Expression = {
        if (prec >= BinaryOps.length) {
            parseUnary()
        } else {
            parseBinary(prec)
        }
    }

    private[this] def parseUnary(): AST.Expression = {
        parsePostfix(parsePrefix(Nil)) { pfx =>
            pfx.foldLeft(AST.Expression(Right(parsePrimary()))) { (v, p) =>
                val Token.Operator(op) = p
                AST.Expression(Left(v), Some(AST.Operator(op)(p)))(p)
            }
        }
    }

    @tailrec
    private[this] def parsePrefix(ret: List[Token]): List[Token] = {
        peek match {
            case Token.Operator(v) if UnaryOps(v) => parsePrefix(next :: ret)
            case Token.Operator(PrefixOps(v))     => Token.Operator(v)(next) :: ret
            case _                                => ret
        }
    }

    private[this] def parsePostfix(pfx: List[Token])(fn: List[Token] => AST.Expression): AST.Expression = {
        parsePostfix(fn(pfx), pfx.headOption)
    }

    private[this] def parsePostfix(ret: AST.Expression, head: Option[Token]): AST.Expression = {
        (peek, head) match {
            case (AsPostfix(_), Some(IsPrefix(_))) => throw SyntaxError("value modified twice")
            case (AsPostfix(v), _)                 => AST.Expression(Left(ret), Some(AST.Operator(v)))(next)
            case _                                 => ret
        }
    }

    private[this] def parseBinary(prec: Int): AST.Expression =  {
        save { implicit p =>
            parseBinaryPart(AST.Expression(Left(parseExpr(prec + 1))), prec)
        }
    }

    @tailrec
    private[this] def parseBinaryPart(ret: AST.Expression, prec: Int): AST.Expression = {
        peek match {
            case t @ Token.Operator(v) if BinaryOps(prec)(v) => parseBinaryPart(parseBinaryTerm(prec + 1, t, ret), prec)
            case _                                           => ret
        }
    }

    private[this] def parseBinaryTerm(prec: Int, t: Token.Operator, ret: AST.Expression): AST.Expression = {
        save { implicit p => drop {
            val Token.Operator(op) = t
            AST.Expression(Left(ret), Some(AST.Operator(op)(t)), Some(parseExpr(prec)))
        }}
    }

    /** Expression Prunning **/

    private[this] def trimExpressionTree(expr: AST.Expression): AST.Expression = {
        (expr.left.left.map(trimExpressionTree), expr.op, expr.right.map(trimExpressionTree)) match {
            case (Left(v), None, None)                                      => v
            case (Right(AST.Primary(v: AST.Expression, Seq())), None, None) => v
            case (left, op, right)                                          => AST.Expression(left, op, right)(expr)
        }
    }
}
