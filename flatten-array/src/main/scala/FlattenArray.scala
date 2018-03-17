object FlattenArray {
  // Implementation that avoids using flatMap
  def flatten(list: List[_]): List[Any] =
    list match {
      case Nil => Nil
      case null :: t => flatten(t)
      case (innerList: List[_]) :: t => flatten(innerList) ++ flatten(t)
      case otherElem :: t => otherElem :: flatten(t)
    }

  // Alternative implementation if flatMap is allowed
  def flattenUsingFlatMap(list: List[_]): List[Any] =
    list.flatMap(elem => elem match {
      case null => List()
      case innerList: List[_] => flattenUsingFlatMap(innerList)
      case _ => List(elem)
    })
}
