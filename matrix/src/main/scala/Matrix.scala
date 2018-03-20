/** Represents a (mathematical) matrix of integers */
final class Matrix private (rows: Seq[Seq[Int]]) {
  private val cols = rows.transpose
  require(rows.nonEmpty && cols.nonEmpty)

  /** Returns the specified row of this Matrix (0-indexed) */
  def row(rowNum: Int): Seq[Int] = rows(rowNum)

  /** Returns the specified column of this Matrix (0-indexed) */
  def column(rowNum: Int): Seq[Int] = cols(rowNum)
}

object Matrix {
  /** Parses a string of the format specified in the README into a Matrix */
  def apply(str: String): Matrix = {
    require(str.nonEmpty)
    val rows: Seq[Seq[Int]] = try {
      str.lines.map(_.split(' ').map(_.toInt).toSeq).toSeq
    } catch {
      case _: NumberFormatException => throw new IllegalArgumentException(
        s"""String "$str" contains non-integer.""")
    }
    if (rows.isEmpty) {
      throw new IllegalArgumentException(
        s"""String "$str" must have at least one row.""")
    }
    if (rows.exists(_.length != rows.head.length)) {
      throw new IllegalArgumentException(
        s"""String "$str" must be a rectangular grid (a matrix).""")
    }
    new Matrix(rows)
  }
}