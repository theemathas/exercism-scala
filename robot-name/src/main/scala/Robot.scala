import scala.util.Random

class Robot() {
  private var _name: String = Robot.genName()

  def name: String = _name

  def reset(): Unit = {
    _name = Robot.genName()
  }
}

object Robot {
  private var unusedNames: Iterator[String] = {
    val letters = 'A' to 'Z'
    val digits = '0' to '9'
    val allNames = for {
      ch1 <- letters
      ch2 <- letters
      ch3 <- digits
      ch4 <- digits
      ch5 <- digits
    } yield Seq(ch1, ch2, ch3, ch4, ch5).mkString
    Random.shuffle(allNames).toIterator
  }

  /** Generates a random name that is guaranteed to be unique */
  private def genName(): String = {
    if (unusedNames.hasNext)
      unusedNames.next
    else
      throw new IllegalStateException("No more unused names.")
  }
}
