object Anagram {
  def anagrams(needle: String, haystack: Seq[String]): Seq[String] = {
    val lowerNeedle = needle.toLowerCase
    val sortedNeedle = lowerNeedle.sorted
    haystack.filter(word => {
      val lowerWord = word.toLowerCase
      lowerWord != lowerNeedle && lowerWord.sorted == sortedNeedle
    })
  }
}
