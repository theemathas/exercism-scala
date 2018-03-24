case class WordCount(private val str: String) {
  lazy val countWords: Map[String, Int] = {
    val words = """'?[^a-z0-9']'?""".r.split(str.toLowerCase).filter(_.nonEmpty)
    words.groupBy(identity).mapValues(_.length)
  }
}
