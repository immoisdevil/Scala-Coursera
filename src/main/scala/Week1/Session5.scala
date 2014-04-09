package Week1

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/7/14
 * Time: 3:11 PM
 * To change this template use File | Settings | File Templates.
 */
object Session5 {
  /**
   * Method to take the absolute of a input
   * @param x the input double value
   * @return
   */
  def abs(x: Double) = if (x >= 0) x else -1*x

  /**
   * Method to get the iterative square root
   * @param guess the middle steps guess value for Newton's method
   * @param value the value for which the sqaure root has to be found
   * @return
   */
  def sqrIter(guess: Double, value: Double): Double = {
    if (isGoodEnough(guess,value)) guess
    else sqrIter(improve(guess,value),value)
  }

  /**
   * Method to check if the guess is good enough
   * @param guess guess value
   * @param value the value for which guess has to be checked
   * @return
   */
  def isGoodEnough(guess: Double, value: Double): Boolean = abs(value - guess*guess) / value <= .001

  /**
   * Method to improve the previous gess
   * @param guess take the guess value to improve it to better guess
   * @param value value for which guess has to be improved
   * @return improved guess
   */
  def improve(guess: Double, value: Double): Double = (guess + value/guess) /2

  /**
   * Actual method to be exposed
   * @param x The Double value for which square root has to be found
   * @return The closest square root value
   */
  def sqrt(x: Double) = sqrIter(1.0, x)

  def main(args:Array[String]){
    println(sqrt(1e-30))
    val x = 5 + 7 +
    9
    println(x)
  }
}
