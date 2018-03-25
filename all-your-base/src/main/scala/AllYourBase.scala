object AllYourBase {
  def rebase(oldBase: Int, oldDigits: Seq[Int], newBase: Int): Option[Seq[Int]] =
    fromBase(oldBase, oldDigits).flatMap(toBase(newBase, _))

  private def fromBase(oldBase: Int, oldDigits: Seq[Int]): Option[Int] = {
    def recur(oldRevDigits: List[Int]): Option[Int] = oldRevDigits match {
      case Nil => Some(0)
      case head :: tail =>
        if (0 <= head && head < oldBase)
          recur(tail).map(_ * oldBase + head)
        else
          None
    }

    if (oldBase >= 2)
      recur(oldDigits.reverse.toList)
    else
      None
  }

  private def toBase(newBase: Int, oldInt: Int): Option[Seq[Int]] = {
    def recur(oldInt: Int): Seq[Int] = {
      if (oldInt == 0)
        Seq.empty
      else
        recur(oldInt / newBase) :+ (oldInt % newBase)
    }

    require(oldInt >= 0)
    if (newBase >= 2)
      Some(recur(oldInt).padTo(1, 0))
    else
      None
  }
}
