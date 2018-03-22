object BinarySearch {
  /** Finds an item in a sorted Seq, and returns its index. If the item is
    * not found, then returns None.
    */
  def find[T](haystack: Seq[T], needle: T)
             (implicit ord: Ordering[T]): Option[Int] = {
    var lo = 0
    var hi = haystack.length
    // Loop invariant: if needle is in haystack, then its index is in [lo, hi)
    while (lo < hi) {
      // Better than "(lo + hi) / 2", since it avoids overflow
      val mid = lo + (hi - lo) / 2
      ord.compare(needle, haystack(mid)) match {
        case 0 => return Some(mid)
        case x if x > 0 => lo = mid + 1
        case x if x < 0 => hi = mid
      }
    }
    None
  }
}
