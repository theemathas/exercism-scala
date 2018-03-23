import scala.collection.mutable.ArrayBuffer

object Sieve {
  def primes(upperBound: Int): Seq[Int] = {
    // The indices go from 0,1,...,upperBound. Index 0 is not used.
    // isPrime(x) is true iff, as far as we know, x is prime.
    // isPrime(x) is false iff x is definitely not prime.
    val isPrime = ArrayBuffer.fill(upperBound + 1)(true)
    isPrime(0) = false
    isPrime(1) = false
    val result = new ArrayBuffer[Int]()
    for (i <- 2 to upperBound) {
      if (isPrime(i)) {
        result += i
        for (j <- (i * 2) to upperBound by i) {
          isPrime(j) = false
        }
      }
    }
    result
  }
}
