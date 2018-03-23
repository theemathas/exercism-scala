object BeerSong {
  def recite(fromNumBottles: Int, numVerses: Int): String =
    (fromNumBottles until (fromNumBottles - numVerses) by -1)
      .map(verse).mkString("\n")

  def verse(numBottles: Int): String = {
    require(numBottles >= 0)
    val line1 =
      s"${beer(numBottles).capitalize} on the wall, ${beer(numBottles)}.\n"
    val line2 =
      if (numBottles == 0) {
        "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
      } else {
        val takeDown = if (numBottles == 1) "it" else "one"
        s"Take $takeDown down and pass it around, ${beer(numBottles - 1)} on the wall.\n"
      }
    line1 ++ line2
  }

  private def beer(numBottles: Int): String = numBottles match {
    case 0 => "no more bottles of beer"
    case 1 => "1 bottle of beer"
    case _ => s"$numBottles bottles of beer"
  }
}
