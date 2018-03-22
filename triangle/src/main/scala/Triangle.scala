final case class Triangle[T](a: T, b: T, c: T)(implicit n: Numeric[T]) {
  import n._

  private def isValid: Boolean =
    a > zero && b > zero && c > zero && a + b > c && a + c > b && b + c > a

  def equilateral: Boolean = isValid && a == b && b == c

  def isosceles: Boolean = isValid && (a == b || a == c || b == c)

  def scalene: Boolean = isValid && !isosceles
}
