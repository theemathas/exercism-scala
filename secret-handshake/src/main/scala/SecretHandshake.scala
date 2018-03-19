object SecretHandshake {
  private val maxBinaryDigits = 5
  private val codes = Seq("wink", "double blink", "close your eyes", "jump")

  /** Encode the number x as a secret handshake. */
  def commands(x: Int): Seq[String] = {
    require(0 <= x && x < (1 << maxBinaryDigits))
    val binary = toBinary(x).padTo(maxBinaryDigits, false)
    val handshake = for {
      (digit, code) <- binary zip codes
      if digit
    } yield code
    val mustReverse = binary.last
    if (mustReverse) handshake.reverse else handshake
  }

  /** Converts a non-negative number into binary, least-significant–digit first.
    * True represents 1, and false represents 0.
    */
  private def toBinary(x: Int): Seq[Boolean] = {
    require(x >= 0)
    x.toBinaryString.reverse.map(_ == '1')
  }
}
