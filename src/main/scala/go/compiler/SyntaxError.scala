package go.compiler

import go.utils.Location

case class SyntaxError(msg: String)(implicit p: Location)
extends RuntimeException(s"${p.file}:${p.row + 1}:${p.col + 1}: $msg") with Location {
    override val col  : Int    = p.col
    override val row  : Int    = p.row
    override val file : String = p.file
}
