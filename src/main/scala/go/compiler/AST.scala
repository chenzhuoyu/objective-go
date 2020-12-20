package go.compiler

import go.utils.Location

sealed abstract class AST()(implicit p: Location)
extends Location.Snapshot

object AST {
    sealed trait Type
    sealed trait Element
    sealed trait Operand
    sealed trait Literal extends Operand
    sealed trait Modifier
    sealed trait Statement
    sealed trait CompositeType
    sealed trait SimpleStatement extends Statement

    sealed trait Variance
    sealed trait ReceiverMode

    case object Invariant     extends Variance
    case object Covariant     extends Variance
    case object Contravariant extends Variance

    case object ValueReceiver     extends ReceiverMode
    case object PointerReceiver   extends ReceiverMode
    case object ReferenceReceiver extends ReceiverMode

    /** AST Primitives **/

    case class Nil      ()                         (implicit p: Location) extends AST with Literal
    case class Name     (name: String)             (implicit p: Location) extends AST with Operand
    case class Operator (kind: Token.OperatorType) (implicit p: Location) extends AST

    case class BoolLit    (v: Boolean)     (implicit p: Location) extends AST with Literal
    case class RuneLit    (v: Int)         (implicit p: Location) extends AST with Literal
    case class FloatLit   (v: Double)      (implicit p: Location) extends AST with Literal
    case class StringLit  (v: Array[Byte]) (implicit p: Location) extends AST with Literal
    case class IntegerLit (v: BigInt)      (implicit p: Location) extends AST with Literal
    case class ComplexLit (v: Double)      (implicit p: Location) extends AST with Literal

    /** Types **/

    case class MapType      (key: Type, elem: Type)                 (implicit p: Location) extends AST with Type with CompositeType
    case class SliceType    (elem: Type)                            (implicit p: Location) extends AST with Type with CompositeType
    case class ArrayType    (elem: Type, len: Option[Expression])   (implicit p: Location) extends AST with Type with CompositeType
    case class ChannelType  (elem: Type, in: Boolean, out: Boolean) (implicit p: Location) extends AST with Type
    case class PointerType  (base: Type)                            (implicit p: Location) extends AST with Type
    case class FunctionType (fsig: FunctionSignature)               (implicit p: Location) extends AST with Type

    case class EnumType(
        vals: Seq[EnumItem],
        tags: Seq[Annotation],
    )(implicit p: Location) extends AST with Type

    case class ClassType(
        base   : Option[TypeName],
        intf   : Seq[TypeName],
        tags   : Seq[Annotation],
        fields : Seq[StructField],
    )(implicit p: Location) extends AST with Type

    case class StructType(
        decl: Seq[StructField],
        tags: Seq[Annotation],
    )(implicit p: Location) extends AST with Type with CompositeType

    case class InterfaceType(
        tags    : Seq[Annotation],
        bases   : Seq[TypeName],
        methods : Seq[InterfaceMethod],
    )(implicit p: Location) extends AST with Type

    case class AnnotationType(
        tags   : Seq[Annotation],
        fields : Seq[AnnotationField],
    )(implicit p: Location) extends AST with Type

    case class AnnotationField(
        name: Name,
        kind: Type,
        tags: Seq[Annotation],
        defv: Option[Expression],
    )(implicit p: Location) extends AST with Type

    case class TypeName(
        name  : Name,
        impl  : Seq[Type],
        scope : Option[Name],
    )(implicit p: Location) extends AST with Type with CompositeType

    case class EnumItem(
        name  : Name,
        tags  : Seq[Annotation],
        value : Seq[Option[Expression]],
    )(implicit p: Location) extends AST

    case class StructField(
        kind: Type,
        name: Option[Name],
        tags: Seq[Annotation],
    )(implicit p: Location) extends AST

    case class InterfaceMethod(
        name  : Name,
        fsig  : FunctionSignature,
        tags  : Seq[Annotation],
        types : Seq[GenericSpec],
    )(implicit p: Location) extends AST

    case class FunctionArgument(
        kind: Type,
        name: Option[Name],
    )(implicit p: Location) extends AST

    case class FunctionSignature(
        varg: Boolean,
        args: Seq[FunctionArgument],
        rets: Seq[FunctionArgument],
    )(implicit p: Location) extends AST

    /** Annotations **/

    case class Annotation(
        name: TypeName,
        args: Seq[AnnotationArg],
    )(implicit p: Location) extends AST

    case class AnnotationArg(
        vals: Literal,
        name: Option[Name],
    )(implicit p: Location) extends AST

    /** Expressions **/

    case class Primary(
        vv   : Operand,
        mods : Seq[Modifier],
    )(implicit p: Location) extends AST

    case class Lambda(
         fsig : FunctionSignature,
         body : Either[Expression, Block],
    )(implicit p: Location) extends AST with Operand

    case class Composite(
        vtype: CompositeType,
        value: CompositeValue,
    )(implicit p: Location) extends AST with Operand

    case class Construct(
        name: TypeName,
        args: Option[Invoke],
    )(implicit p: Location) extends AST with Operand

    case class Expression(
        left  : Either[Expression, Primary],
        op    : Option[Operator] = None,
        right : Option[Expression] = None,
    )(implicit p: Location) extends AST with Operand with Element with SimpleStatement

    case class FunctionCall(
        name: Name,
        args: Invoke,
    )(implicit p: Location) extends AST with Operand

    case class CompositeValue   (vv: Seq[CompositeElement])            (implicit p: Location) extends AST with Element
    case class CompositeElement (key: Option[Element], value: Element) (implicit p: Location) extends AST

    /** Expression Modifiers **/

    case class Index      (expr: Expression) (implicit p: Location) extends AST with Modifier
    case class Selector   (attr: Name)       (implicit p: Location) extends AST with Modifier
    case class Conversion (dest: Type)       (implicit p: Location) extends AST with Modifier

    case class Slice(
        pos: Option[Expression],
        len: Option[Expression],
        cap: Option[Expression],
    )(implicit p: Location) extends AST with Modifier

    case class Invoke(
        varg: Boolean,
        impl: Seq[Type],
        args: Seq[Expression],
    )(implicit p: Location) extends AST with Modifier

    case class Method(
        name: Name,
        args: Invoke,
    )(implicit p: Location) extends AST with Modifier

    /** Top Level Declarations **/

    case class Import(
        path  : StringLit,
        items : Seq[ImportName],
    )(implicit p: Location) extends AST

    case class ImportName(
        name  : Name,
        alias : Option[Name],
    )(implicit p: Location) extends AST

    case class TypeSpec(
        name  : Name,
        vtype : Type,
        alias : Boolean,
        types : Seq[GenericSpec],
    )(implicit p: Location) extends AST with Statement

    case class ValueSpec(
        vtype  : Type,
        names  : Seq[Name],
        values : Seq[Expression],
        consts : Boolean,
    )(implicit p: Location) extends AST with SimpleStatement

    case class GenericSpec(
        name  : Name,
        mode  : Variance,
        lower : Option[TypeName],
        upper : Option[TypeName],
    )(implicit p: Location) extends AST with SimpleStatement

    case class Function(
        name  : Name,
        body  : Option[Block],
        tags  : Seq[Annotation],
        fsig  : FunctionSignature,
        recv  : Option[FunctionReceiver],
        types : Seq[GenericSpec],
    )(implicit p: Location) extends AST

    case class FunctionReceiver(
        kind: Name,
        name: Option[Name],
        mode: ReceiverMode,
    )(implicit p: Location) extends AST

    case class Decorator(
        term: Primary,
        body: Either[Function, Decorator],
    )(implicit p: Location) extends AST

    case class Package(
        name    : Name,
        vars    : Seq[ValueSpec],
        decos   : Seq[Decorator],
        funcs   : Seq[Function],
        types   : Seq[TypeSpec],
        consts  : Seq[ValueSpec],
        imports : Seq[Import],
    )(implicit p: Location) extends AST

    /** Statements **/

    sealed trait ElseIf
    sealed trait TermSvd
    sealed trait CaseBody
    sealed trait SelectOps

    case class Svd  (vals: Seq[Name])       (implicit p: Location) extends AST with TermSvd
    case class Term (vals: Seq[Expression]) (implicit p: Location) extends AST with TermSvd

    case class Go       (expr: Expression)      (implicit p: Location) extends AST with Statement
    case class Defer    (expr: Expression)      (implicit p: Location) extends AST with Statement
    case class Block    (body: Seq[Statement])  (implicit p: Location) extends AST with Statement with ElseIf with CaseBody
    case class Label    (name: Name)            (implicit p: Location) extends AST with Statement
    case class Break    (dest: Option[Name])    (implicit p: Location) extends AST with Statement with CaseBody
    case class Return   (vals: Seq[Expression]) (implicit p: Location) extends AST with Statement
    case class Select   (iops: Seq[SelectCase]) (implicit p: Location) extends AST with Statement
    case class Continue (dest: Option[Name])    (implicit p: Location) extends AST with Statement

    case class Send(
        chan: Expression,
        expr: Expression,
    )(implicit p: Location) extends AST with SimpleStatement with SelectOps

    case class Assign(
        kind: Operator,
        vals: Seq[(Primary, Expression)]
    )(implicit p: Location) extends AST with SimpleStatement

    case class If(
        body   : Block,
        cond   : Expression,
        init   : Option[SimpleStatement],
        branch : Option[ElseIf],
    )(implicit p: Location) extends AST with Statement with ElseIf

    case class For(
        body: Block,
        cond: Option[Expression],
        init: Option[SimpleStatement],
        post: Option[SimpleStatement],
    )(implicit p: Location) extends AST with Statement

    case class ForRange(
        body: Block,
        dest: TermSvd,
        expr: Expression,
    )(implicit p: Location) extends AST with Statement

    case class Switch(
        expr  : Expression,
        init  : Option[SimpleStatement],
        cases : Seq[SwitchCase],
    )(implicit p: Location) extends AST with Statement

    case class SwitchCase(
        expr: Expression,
        body: Option[CaseBody],
    )(implicit p: Location) extends AST with Statement

    case class SelectCase(
        body: Block,
        expr: Option[SelectOps]
    )(implicit p: Location) extends AST with Statement

    case class SelectRecv(
        dest  : TermSvd,
        value : Expression,
    )(implicit p: Location) extends AST with Statement with SelectOps
}
