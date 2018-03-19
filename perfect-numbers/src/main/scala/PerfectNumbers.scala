object PerfectNumbers {
  def classify(x: Int): Either[String, NumberType] = {
    if (x <= 0) {
      Left("Classification is only possible for natural numbers.")
    } else Right {
      val sum = sumFactors(x)
      if (sum == x)
        NumberType.Perfect
      else if (sum > x)
        NumberType.Abundant
      else
        NumberType.Deficient
    }
  }

  /** Returns the sum of positive factors of x, excluding x */
  private def sumFactors(x: Int): Int = {
    require(x > 0)
    (1 until x).filter(x % _ == 0).sum
  }
}

sealed trait NumberType

object NumberType {
  case object Perfect extends NumberType
  case object Abundant extends NumberType
  case object Deficient extends NumberType
}
