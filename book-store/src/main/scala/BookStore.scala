object BookStore {
  /** prices(x) = price of x different books, in cents */
  private val prices = Seq(
    0, 800, // 8 dollars for one book
    800 * 2 * 95 / 100, // 5% off for two books
    800 * 3 * 90 / 100, // 10% off for three books
    800 * 4 * 80 / 100, // 20% off for four books
    800 * 5 * 75 / 100  // 25% off for five books
  )
  private val numBookTitles = 5
  assert(prices.length == numBookTitles + 1)

  /** Computes the price of a set of book purchases.
    *
    * @param purchases A collection of books, with each Int representing one
    *                  copy of a book. It should contain only numbers in 1-5,
    *                  with 1 representing the first title,..., 5 representing
    *                  the fifth title.
    * @return The total price of the books, in cents.
    */
  def total(purchases: Traversable[Int]): Int = {

    require(purchases.forall(x => 1 <= x && x <= numBookTitles))
    // The number of copies for each book title
    val bookCounts: Seq[Int] = purchases.groupBy(identity).map(_._2.size).toSeq
    assert(bookCounts.size <= numBookTitles)
    search(bookCounts, numBookTitles)
  }

  /** Returns the minimum price for a given bookCounts, using prices only from
    *  prices(1),...,prices(nextGroupSize)
    */
  private def search(bookCounts: Seq[Int], nextGroupSize: Int): Int = {
    require(bookCounts.forall(_ > 0))
    if (nextGroupSize == 1) {
      bookCounts.sum * prices(1)
    } else {
      var remaining = bookCounts
      var minPrice = search(remaining, nextGroupSize - 1)
      var removedPrice = 0
      // While can do removeGroup
      while (remaining.length >= nextGroupSize) {
        remaining = removeGroup(remaining, nextGroupSize)
        removedPrice += prices(nextGroupSize)
        minPrice = minPrice.min(
          removedPrice + search(remaining, nextGroupSize - 1))
      }
      minPrice
    }
  }

  /** Subtract 1 from the groupSize largest elements in bookCounts, and remove
    * any zeros.
    */
  private def removeGroup(bookCounts: Seq[Int], groupSize: Int): Seq[Int] = {
    require(bookCounts.forall(_ > 0))
    require(groupSize <= bookCounts.length)
    val sorted = bookCounts.sorted.reverse
    val subtracted = for {
      (count, index) <- bookCounts.zipWithIndex
    } yield if (index < groupSize) count-1 else count
    subtracted.filter(_ != 0)
  }
}
