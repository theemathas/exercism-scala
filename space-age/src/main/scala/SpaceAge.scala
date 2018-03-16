/** Converts time in seconds to time in Earth-years, Mercury-years, etc.
  * Return values are rounded to two decimal places
  */
object SpaceAge {
  private val SecondsPerEarthYear: Double = 365.25 * 24 * 60 * 60

  /** Rounds a number to two decimal places */
  private def round(x: Double): Double = (x * 100.0).round / 100.0

  /** Converts time in seconds to time in years on a target planet.
    *
    * @param seconds The time to be converted, in seconds.
    * @param earthYearsPerYear The number of earth years equivalent to one year
    *                          on the target planet.
    * @return The time converted into number of years on the target planet,
    *         rounded to two decimal places.
    */
  private def onPlanet(seconds: Double, earthYearsPerYear: Double) =
    round(seconds / SecondsPerEarthYear / earthYearsPerYear)

  def onEarth(seconds: Double): Double =
    onPlanet(seconds, 1.0)

  def onMercury(seconds: Double): Double =
    onPlanet(seconds, 0.2408467)

  def onVenus(seconds: Double): Double =
    onPlanet(seconds, 0.61519726)

  def onMars(seconds: Double): Double =
    onPlanet(seconds, 1.8808158)

  def onJupiter(seconds: Double): Double =
    onPlanet(seconds, 11.862615)

  def onSaturn(seconds: Double): Double =
    onPlanet(seconds, 29.447498)

  def onUranus(seconds: Double): Double =
    onPlanet(seconds, 84.016846)

  def onNeptune(seconds: Double): Double =
    onPlanet(seconds, 164.79132)
}
