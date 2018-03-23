object Isogram {
  def isIsogram(str: String): Boolean = {
    var chars = str.filter(_.isLetter).toLowerCase
    chars.toSet.size == chars.length
  }
}
