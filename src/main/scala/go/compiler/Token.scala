package go.compiler

import go.utils.Location

import scala.Predef.{String => PString}

sealed abstract class Token()(implicit p: Location)
extends Location.Snapshot

object Token {
    sealed trait KeywordType
    sealed trait OperatorType
    sealed abstract class Directive(implicit val p: Location) extends Token

    case object Break     extends KeywordType
    case object Case      extends KeywordType
    case object Catch     extends KeywordType
    case object Chan      extends KeywordType
    case object Class     extends KeywordType
    case object Const     extends KeywordType
    case object Continue  extends KeywordType
    case object Default   extends KeywordType
    case object Defer     extends KeywordType
    case object Else      extends KeywordType
    case object Enum      extends KeywordType
    case object Extends   extends KeywordType
    case object False     extends KeywordType
    case object Finally   extends KeywordType
    case object For       extends KeywordType
    case object From      extends KeywordType
    case object Func      extends KeywordType
    case object Go        extends KeywordType
    case object If        extends KeywordType
    case object Import    extends KeywordType
    case object In        extends KeywordType
    case object Interface extends KeywordType
    case object New       extends KeywordType
    case object Nil       extends KeywordType
    case object Override  extends KeywordType
    case object Package   extends KeywordType
    case object Return    extends KeywordType
    case object Select    extends KeywordType
    case object Struct    extends KeywordType
    case object Super     extends KeywordType
    case object Switch    extends KeywordType
    case object Throw     extends KeywordType
    case object True      extends KeywordType
    case object Try       extends KeywordType
    case object Type      extends KeywordType
    case object With      extends KeywordType
    case object Var       extends KeywordType

    case object `+`   extends OperatorType
    case object `-`   extends OperatorType
    case object `*`   extends OperatorType
    case object `/`   extends OperatorType
    case object `%`   extends OperatorType
    case object `&`   extends OperatorType
    case object `|`   extends OperatorType
    case object `^`   extends OperatorType
    case object `~`   extends OperatorType
    case object `<<`  extends OperatorType
    case object `>>`  extends OperatorType
    case object `+=`  extends OperatorType
    case object `-=`  extends OperatorType
    case object `*=`  extends OperatorType
    case object `/=`  extends OperatorType
    case object `%=`  extends OperatorType
    case object `&=`  extends OperatorType
    case object `|=`  extends OperatorType
    case object `^=`  extends OperatorType
    case object `~=`  extends OperatorType
    case object `<<=` extends OperatorType
    case object `>>=` extends OperatorType
    case object `&&`  extends OperatorType
    case object `||`  extends OperatorType
    case object `!`   extends OperatorType
    case object `=>`  extends OperatorType
    case object `<-`  extends OperatorType
    case object `++`  extends OperatorType
    case object `--`  extends OperatorType
    case object `==`  extends OperatorType
    case object `<`   extends OperatorType
    case object `>`   extends OperatorType
    case object `!=`  extends OperatorType
    case object `<=`  extends OperatorType
    case object `>=`  extends OperatorType
    case object `=`   extends OperatorType
    case object `:=`  extends OperatorType
    case object `...` extends OperatorType
    case object `(`   extends OperatorType
    case object `)`   extends OperatorType
    case object `[`   extends OperatorType
    case object `]`   extends OperatorType
    case object `{`   extends OperatorType
    case object `}`   extends OperatorType
    case object `,`   extends OperatorType
    case object `.`   extends OperatorType
    case object `;`   extends OperatorType
    case object `:`   extends OperatorType
    case object `?`   extends OperatorType

    case class Prefix(op: OperatorType) extends OperatorType
    case class Postfix(op: OperatorType) extends OperatorType

    case class LF        ()                (implicit val p: Location) extends Token
    case class End       ()                (implicit val p: Location) extends Token
    case class Nil       ()                (implicit val p: Location) extends Token
    case class Bool      (v: Boolean)      (implicit val p: Location) extends Token
    case class Name      (v: PString)      (implicit val p: Location) extends Token
    case class Rune      (v: Int)          (implicit val p: Location) extends Token
    case class Float     (v: Double)       (implicit val p: Location) extends Token
    case class String    (v: Array[Byte])  (implicit val p: Location) extends Token
    case class Integer   (v: BigInt)       (implicit val p: Location) extends Token
    case class Complex   (v: Double)       (implicit val p: Location) extends Token
    case class Keyword   (v: KeywordType)  (implicit val p: Location) extends Token
    case class Operator  (v: OperatorType) (implicit val p: Location) extends Token

    case class NoSplit  ()                             (override implicit val p: Location) extends Directive
    case class NoEscape ()                             (override implicit val p: Location) extends Directive
    case class LinkName (name: PString, link: PString) (override implicit val p: Location) extends Directive
}
