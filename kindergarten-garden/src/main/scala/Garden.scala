object Plant extends Enumeration {
  type Plant = Value
  val Grass, Clover, Radishes, Violets = Value
}
import Plant._

class Garden private (row1: Seq[Plant], row2: Seq[Plant]) {
  def plants(childName: String): Seq[Plant] = {
    val childIndex = Garden.childrenNames.indexOf(childName)
    Seq(
      row1(childIndex * 2), row1(childIndex * 2 + 1),
      row2(childIndex * 2), row2(childIndex * 2 + 1)
    )
  }
}

object Garden {
  private val childrenNames = Seq(
    "Alice", "Bob", "Charlie", "David",
    "Eve", "Fred", "Ginny", "Harriet",
    "Ileana", "Joseph", "Kincaid", "Larry"
  )

  private val charToPlant = Map(
    'G' -> Grass,
    'C' -> Clover,
    'R' -> Radishes,
    'V' -> Violets
  )

  def defaultGarden(str: String): Garden = {
    val lines = str.lines.toArray
    require(lines.length == 2)
    new Garden(lines(0).map(charToPlant), lines(1).map(charToPlant))
  }
}
