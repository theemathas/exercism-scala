object Grains {
  def square(squareId: Int): Option[BigInt] = {
    if (squareId >= 1 && squareId <= 64)
      Some(BigInt(2).pow(squareId - 1))
    else
      None
  }

  val total: BigInt = (1 to 64).map(square(_).get).sum
}
