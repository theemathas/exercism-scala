import scala.collection.mutable.ListBuffer

object Strain {
  def keep[T](data: Seq[T], f: T => Boolean): Seq[T] = {
    val result = new ListBuffer[T]
    for (x <- data) {
      if (f(x)) {
        result += x
      }
    }
    result
  }

  def discard[T](data: Seq[T], f: T => Boolean): Seq[T] = keep[T](data, !f(_))
}