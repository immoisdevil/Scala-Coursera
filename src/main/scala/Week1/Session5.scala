package Week1

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/7/14
 * Time: 3:11 PM
 * To change this template use File | Settings | File Templates.
 */
object Session5 {

  def abs(x: Double) = if (x >= 0) x else -1*x

  def sqrIter(guess: Double, value: Double): Double = {
    if (isGoodEnough(guess,value)) guess
    else sqrIter(improve(guess,value),value)
  }
  def isGoodEnough(guess: Double, value: Double): Boolean = abs(value - guess*guess) / value <= .001

  def improve(guess: Double, value: Double): Double = (guess + value/guess) /2

  def sqrt(x: Double) = sqrIter(1.0, x)

  def main(args:Array[String]){
    println(sqrt(1e-30))
    val x = 5 + 7 +
    9
    println(x)
  }
}
