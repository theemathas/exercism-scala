import java.time.{DayOfWeek, LocalDate, YearMonth}

import Schedule.Schedule

case class Meetup(month: Int, year: Int) {

  def day(dayOfWeek: DayOfWeek, schedule: Schedule): LocalDate = {
    val days = allDays(dayOfWeek)
    schedule match {
      case Schedule.Teenth =>
        days.find(date => (13 to 19).contains(date.getDayOfMonth)).get
      case Schedule.First  => days(0)
      case Schedule.Second => days(1)
      case Schedule.Third  => days(2)
      case Schedule.Fourth => days(3)
      case Schedule.Last => days.last
    }
  }

  /** All days in the month with the given dayOfWeek */
  private def allDays(dayOfWeek: DayOfWeek): Seq[LocalDate] = {
    (1 to YearMonth.of(year, month).lengthOfMonth)
      .map(LocalDate.of(year, month, _))
      .filter(_.getDayOfWeek == dayOfWeek)
  }
}

object Schedule extends Enumeration {
  type Schedule = Value
  val Teenth, First, Second, Third, Fourth, Last = Value
}

object Meetup {
  val Mon = DayOfWeek.MONDAY
  val Tue = DayOfWeek.TUESDAY
  val Wed = DayOfWeek.WEDNESDAY
  val Thu = DayOfWeek.THURSDAY
  val Fri = DayOfWeek.FRIDAY
  val Sat = DayOfWeek.SATURDAY
  val Sun = DayOfWeek.SUNDAY
}
