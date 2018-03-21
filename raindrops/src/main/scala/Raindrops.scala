object Raindrops {
  def convert(n: Int): String = {
    val str = new StringBuilder
    if (n % 3 == 0) {
      str.append("Pling")
    }
    if (n % 5 == 0) {
      str.append("Plang")
    }
    if (n % 7 == 0) {
      str.append("Plong")
    }
    if (str.isEmpty) n.toString else str.toString
  }
}

