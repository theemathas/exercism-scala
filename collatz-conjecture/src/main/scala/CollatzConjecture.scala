object CollatzConjecture {
  /** Computes the number of steps (either dividing by two, or multiplying by
    * three then adding one) for a number to reach 1 using the procedure in
    * the Collatz conjecture.
    *
    * @param startVal The number to start the process with.
    * @return Some(number of steps) if startNum is positive
    *         None otherwise
    */
  def steps(startVal: Int): Option[Int] =
    if (startVal <= 0) {
      None
    } else {
      var numSteps = 0
      var currVal = startVal
      while (currVal != 1) {
        numSteps += 1
        if (currVal % 2 == 0)
          currVal /= 2
        else
          currVal = currVal * 3 + 1
      }
      Some(numSteps)
    }
}
