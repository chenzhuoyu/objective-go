package go.utils

class MonoBuffer[T](f: => T, private var v: Option[T] = None) {
    def next: T = v.fold(f) { x => v = None; x }
    def peek: T = v.getOrElse { val x = f; v = Some(x); x }
}

object MonoBuffer {
    def apply[T](f: => T): MonoBuffer[T] = {
        new MonoBuffer(f)
    }
}
