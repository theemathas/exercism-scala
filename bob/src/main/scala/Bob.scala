object Bob {
  /** Asks a question to Bob, and returns Bob's response. */
  def response(statement: String): String = {
    val shout = isShout(statement)
    val question = isQuestion(statement)
    if (isSilent(statement)) "Fine. Be that way!"
    else if (shout && question) "Calm down, I know what I'm doing!"
    else if (shout) "Whoa, chill out!"
    else if (question) "Sure."
    else "Whatever."
  }

  /** A string is a shout iff it has at least one upper case character and no
    * lowercase characters.
    */
  private def isShout(statement: String): Boolean =
    statement.exists(_.isUpper) && !statement.exists(_.isLower)

  /** A string is a question iff it ends with a question mark, ignoring
    * whitespace.
    */
  private def isQuestion(statement: String): Boolean =
    statement.trim.endsWith("?")

  /** A string is silent iff it is either empty or only contains whitespace. */
  private def isSilent(statement: String): Boolean =
    statement.forall(_.isWhitespace)
}
