object Hamming {
  /** Computes the hamming distance between two strings, returning None if
    * they have different lengths.
    */
  def distance(x: String, y: String): Option[Int] =
    if (x.length == y.length)
      Some(x.zip(y).count(x => x._1 != x._2))
    else
      None
}
