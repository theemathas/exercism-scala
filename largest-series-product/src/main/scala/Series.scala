import scala.util.Try

object Series {
  def largestProduct(seriesLength: Int, digitString: String): Option[Int] = {
    if (seriesLength > digitString.length ||
        seriesLength < 0 ||
        digitString.exists(!_.isDigit))
      None
    else if (seriesLength == 0)
      Some(1)
    else
      Some {
        digitString
          .map(_.asDigit)
          .sliding(seriesLength)
          .map(_.product)
          .max
      }
  }
}
