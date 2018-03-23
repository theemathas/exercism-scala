object ProteinTranslation {
  private val codonToProtein = Map(
    "AUG" -> "Methionine",
    "UUU" -> "Phenylalanine",
    "UUC" -> "Phenylalanine",
    "UUA" -> "Leucine",
    "UUG" -> "Leucine",
    "UCU" -> "Serine",
    "UCC" -> "Serine",
    "UCA" -> "Serine",
    "UCG" -> "Serine",
    "UAU" -> "Tyrosine",
    "UAC" -> "Tyrosine",
    "UGU" -> "Cysteine",
    "UGC" -> "Cysteine",
    "UGG" -> "Tryptophan"
  )

  private val stopCodons = Set("UAA", "UAG", "UGA")

  def proteins(rna: String): Seq[String] =
    try {
      rna.grouped(3).takeWhile(!stopCodons.contains(_)).map(codonToProtein)
        .toSeq
    } catch {
      case e: NoSuchElementException =>
        throw new IllegalArgumentException(s"Invalid RNA: $rna", e)
    }
}
