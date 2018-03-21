object DifferenceOfSquares {
  private def square(n: Int): Int = n * n

  def sumOfSquares(n: Int): Int = {
    require(n > 0)
    (1 to n).map(square).sum
  }

  def squareOfSum(n: Int): Int = {
    require(n > 0)
    square((1 to n).sum)
  }

  def differenceOfSquares(n: Int): Int = {
    require(n > 0)
    squareOfSum(n) - sumOfSquares(n)
  }
}
