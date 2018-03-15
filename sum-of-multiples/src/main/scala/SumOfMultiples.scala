object SumOfMultiples {
  // Returns the sum of integers in 1,2,...,limit that are divisible by at least
  // one of the numbers in factors
  def sum(factors: Set[Int], limit: Int): Int =
    (1 until limit).filter(x => factors.exists(x % _ == 0)).sum
}

