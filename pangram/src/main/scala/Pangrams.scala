object Pangrams {
  def isPangram(input: String): Boolean =
    input.filter(_.isLetter).toLowerCase.toSet == ('a' to 'z').toSet
}

