object House {
  // Seq of (verb, noun) pairs
  private val items = Seq(
    ("lay in", "the house that Jack built"),
    ("ate", "the malt"),
    ("killed", "the rat"),
    ("worried", "the cat"),
    ("tossed", "the dog"),
    ("milked", "the cow with the crumpled horn"),
    ("kissed", "the maiden all forlorn"),
    ("married", "the man all tattered and torn"),
    ("woke", "the priest all shaven and shorn"),
    ("kept", "the rooster that crowed in the morn"),
    ("belonged to", "the farmer sowing his corn"),
    ("", "the horse and the hound and the horn")
  )

  def recite(startNumItems: Int, toNumItems: Int): String =
    (startNumItems to toNumItems).map(verse).mkString

  private def verse(numItems: Int): String = {
    require(1 <= numItems && numItems <= items.length)
    val usedItems = items.take(numItems).reverse
    val firstLine = s"This is ${usedItems.head._2}"
    val remainingLines =
      usedItems.tail.map(item => s"\nthat ${item._1} ${item._2}").mkString
    firstLine ++ remainingLines ++ ".\n\n"
  }
}
