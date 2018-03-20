/** A Robot is immutable. It has a bearing (the cardinal direction it is
  * facing towards) and a pair of (x, y) integer coordinates for its position.
  */
case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def turnRight: Robot = Robot(bearing.nextClockwise, coordinates)

  def turnLeft: Robot = Robot(bearing.nextCounterclockwise, coordinates)

  def advance: Robot = {
    val dir = bearing.unitVector
    Robot(bearing, (coordinates._1 + dir._1, coordinates._2 + dir._2))
  }

  /** Does a series of turnRight/turnLeft/advance instructions according to
    * the instructions string.
    *
    * @param instructions A series of instructions, which must be composed of
    *                     'R', 'L', and 'A'.
    * @return The Robot that results from following the instructions,
    *         starting from this Robot.
    */
  def simulate(instructions: String): Robot = instructions.foldLeft(this) {
    case (robot, 'R') => robot.turnRight
    case (robot, 'L') => robot.turnLeft
    case (robot, 'A') => robot.advance
    case _ =>
      throw new IllegalArgumentException(
        s"""Instructions "$instructions" contains invalid character""")
  }
}

sealed trait Bearing {
  import Bearing._

  def nextClockwise: Bearing = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }

  def nextCounterclockwise: Bearing = this match {
    case North => West
    case East => North
    case South => East
    case West => South
  }

  /** The coordinates one would end up at, if one starts from (0, 0), and
    * travels one unit in this direction.
    */
  def unitVector: (Int, Int) = this match {
    case North => (0, 1)
    case East => (1, 0)
    case South => (0, -1)
    case West => (-1, 0)
  }
}

object Bearing {
  case object North extends Bearing
  case object East extends Bearing
  case object South extends Bearing
  case object West extends Bearing
}
