object Etl {
  def transform(oldFormat: Map[Int, Seq[String]]): Map[String, Int] = {
    for {
      (score, letters) <- oldFormat
      letter <- letters
    } yield (letter.toLowerCase, score)
  }
}
