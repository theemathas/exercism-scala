object Allergen extends Enumeration {
  type Allergen = Value
  val Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen,
    Cats = Value
}
import Allergen._

object Allergies {
  def allergicTo(allergen: Allergen, score: Int): Boolean =
    (score & (1 << allergen.id)) != 0

  def list(score: Int): Seq[Allergen] =
    Allergen.values.toSeq.filter(allergicTo(_, score))
}

