object RnaTranscription {
  def toRna(dna: String): Option[String] = Some(dna.map {
    case 'G' => 'C'
    case 'C' => 'G'
    case 'T' => 'A'
    case 'A' => 'U'
    case _ => return None
  })
}
