import Math.{floorMod, floorDiv}

object Clock {
  def apply(hours: Int, minutes: Int): Time = {
    Time(floorMod(hours + floorDiv(minutes, 60), 24), floorMod(minutes, 60))
  }
  def apply(minutes: Int): Duration = Duration(minutes)
}

final case class Time private (hours: Int, minutes: Int) {
  require(0 <= hours && hours < 24)
  require(0 <= minutes && minutes < 60)

  def +(d: Duration): Time = Clock(hours, minutes + d.minutes)

  def -(d: Duration): Time = Clock(hours, minutes - d.minutes)
}

final case class Duration(minutes: Int)
