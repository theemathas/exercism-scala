class DNA(nucleotides: String) {
  lazy val nucleotideCounts: Either[InvalidNucleotide, Map[Char, Int]] = {
    if (nucleotides.toSet.subsetOf(Set('A', 'C', 'G', 'T')))
      Right(Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0) ++
        nucleotides.groupBy(identity).mapValues(_.length))
    else
      Left(InvalidNucleotide())
  }
}

case class InvalidNucleotide()
