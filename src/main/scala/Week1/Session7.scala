package Week1

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/7/14
 * Time: 4:32 PM
 * To change this template use File | Settings | File Templates.
 */
object Session7 {
  /**
   * Method to calculate gcd
   * @param x The first parameter for gcd
   * @param y The decond parameter for gcd
   * @return The gcd for the inputs
   */
  def gcd(x: Int , y: Int): Int = if (y == 0) x else gcd(y, x % y)

  /**
   * Method to calculate factorial
   * @param n value for which factorial has to be found
   * @return The factorial of the input
   */
  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n-1)

  /**
   * Method to calculate tail recursive factorial
   * @param n value for which factorial has to be found
   * @return The factorial of the input
   */
  def tailFactorial(n: Int): Int = {
    def loop (acc: Int, n: Int): Int = if (n.equals(0)) acc else loop(acc * n, n-1)
    loop(1,n)
  }

  def main(args: Array[String]){
    val ty = 78
    println(gcd(14,21))
    println(factorial(4))
    println(tailFactorial(4))
  }
}
