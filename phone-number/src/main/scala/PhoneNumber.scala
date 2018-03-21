object PhoneNumber {
  def clean(original: String): Option[String] = {
    val pattern = """1?([2-9]\d\d[2-9]\d{6})""".r
    original.filter(_.isDigit) match {
      case pattern(number) => Some(number)
      case _ => None
    }
  }
}
