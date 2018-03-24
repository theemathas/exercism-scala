object Series {
  def slices(sliceSize: Int, num: String): Seq[Seq[Int]] =
    num.map(_.asDigit).sliding(sliceSize).toSeq
}
