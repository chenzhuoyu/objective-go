package go.utils

import go.compiler.Token.OperatorType

import scala.annotation.tailrec
import scala.collection.mutable

class OpTree(private var v: Option[OperatorType] = None, private val ch: mutable.Map[Int, OpTree] = mutable.Map()) {
    def value            : Option[OperatorType] = v
    def apply(v: Int)    : OpTree               = ch(v)
    def unapply(v: Int)  : Option[OpTree]       = ch.get(v)
    def contains(v: Int) : Boolean              = unapply(v).isDefined
}

object OpTree {
    def apply(vv: (String, OperatorType)*): OpTree = {
        insertAll(new OpTree(), vv)
    }

    @tailrec
    private[this] def insert(p: OpTree, k: Seq[Int], v: OperatorType): Unit = k match {
        case h +: t => insert(p.ch.getOrElseUpdate(h, new OpTree), t, v)
        case _      => p.v = Some(v)
    }

    private[this] def insertAll(p: OpTree, vv: Seq[(String, OperatorType)]): OpTree = {
        vv.foreach(v => insert(p, v._1.map(_.toInt), v._2))
        p
    }
}
