package Week1

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/7/14
 * Time: 4:32 PM
 * To change this template use File | Settings | File Templates.
 */
object Session7 {
  def gcd(x: Int , y: Int): Int = if (y == 0) x else gcd(y, x % y)
  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n-1)
  def main(args: Array[String]){
    val ty = 78
    println(gcd(14,21))
    println(factorial(4))
    println(tailFactorial(4))
  }

  def tailFactorial(n: Int): Int = {
    def loop (acc: Int, n: Int): Int = if (n.equals(0)) acc else loop(acc * n, n-1)
    loop(1,n)
  }
}
