object PrimeFactors {
  def factors(x: Long): Seq[Long] = {
    require(x >= 1)
    var result = Seq.empty[Long]
    var remain = x
    var currFactor = 2L
    while (remain != 1L) {
      if (remain % currFactor == 0L) {
        remain /= currFactor
        result :+= currFactor
      } else {
        currFactor += 1L
      }
    }
    result
  }
}
