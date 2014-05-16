package Week2
import Week1.Session5.abs

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/29/14
 * Time: 11:14 AM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session3 {
  val tolerance = 0.0001

  /**
   *
   * @param value the next value which is to be compared to the initial guess
   * @param guess the initial guess
   * @return      Boolean
   */
  def isGoodEnough(value: Double, guess: Double) :Boolean = if (abs((value - guess)/guess)/guess < tolerance ) true else false

  /**
   *
   * @param f          the function to get the fixed point
   * @param firstGuess the initial guess estimate
   * @return           the value of fixed point
   */
  def fixedPoint(f: Double => Double, firstGuess: Double) = {
    def iterate(guess: Double): Double ={
      val newerGuess = f(guess)
      if(isGoodEnough(newerGuess, guess)) newerGuess else iterate(newerGuess)
    }
    iterate(firstGuess)
  }

  /*
  def fixedPointTail(f: Double => Double, firstGuess: Double) = {
    def iterate(acc: Double, guess : )
  }
  */

  def sqrtFixedPoint(x: Double) = fixedPoint(y =>(y + x/y)/2, 1)

  /**
   * @define method to dampen a value which we get by applying a function f
   * @param f function to be applied on the input parameters to be dampened
   * @param x value x to
   * @return
   */
  def avgDamp(f: Double => Double)(x: Double) = (x + f(x))/2

  def sqrtAvgDamp(x: Double) = fixedPoint(avgDamp(y => x/y), 1)

  def main(args: Array[String])={
    println(fixedPoint(x => 1+x/2, 1))
    println(sqrtFixedPoint(2))
    println(sqrtAvgDamp(2))
  }
}
