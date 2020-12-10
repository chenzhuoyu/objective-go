package go.utils

trait Location {
    def col  : Int
    def row  : Int
    def file : String
}

object Location {
    trait Carrier extends Location {
        implicit final val __location__ : Location = this
    }

    class Snapshot()(implicit p: Location) extends Location {
        override val col  : Int    = p.col
        override val row  : Int    = p.row
        override val file : String = p.file
    }

    object Snapshot {
        def apply()(implicit p: Location): Snapshot = new Snapshot()
    }
}
