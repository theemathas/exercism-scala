import collection.mutable
import scala.collection.immutable.TreeMap

/** A school is a mutable collection of classes (identified by Ints), each of
  * which has associated students (represented as Seq[String]).
  */
final class School {
  type DB = Map[Int, Seq[String]]

  private val _db: mutable.Map[Int, mutable.Buffer[String]] = mutable.Map.empty

  /** Add the student with the given name to the class g. A student with this
    *  name should not already exist in this School.
    */
  def add(name: String, g: Int): Unit = {
    if (!_db.contains(g)) {
      _db(g) = mutable.Buffer.empty
    }
    _db(g) += name
  }

  /** Returns a Map representation of the school, with the student names in
    * each class ordered in the same order as they were added.
    */
  def db: DB = _db.mapValues(_.toSeq).toMap

  /** Returns the students in the given class, ordered in the same order as
    * were added.
    */
  // Do a defensive copy of the returned Seq
  def grade(g: Int): Seq[String] = Seq(_db.getOrElse(g, Seq.empty): _*)

  /** Returns a Map representation of the school, with class numbers ordered
    * in ascending order, and the student names in each class ordered
    * alphabetically.
    */
  // Convert the mutable.Map to a TreeMap so the keys are sorted
  def sorted: DB = TreeMap(_db.mapValues(_.sorted).toSeq: _*)
}
