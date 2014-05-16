package Week2

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/9/14
 * Time: 12:36 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session2 {
  /**
   * a curried mehod for sum function
   * @param f mathematical operator
   * @return functions that take two parameters and gives sum of the operand between them
   */
  def sumCurried(f: (Int) => Int ): (Int, Int) => Int ={
    def sumF(a : Int, b: Int):Int = if (a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }

  /**
   * a mutiple parameter list method curried function for sum
   * @param f the mathematical operator
   * @param a the first input parameter
   * @param b the second parameter
   * @return sum of the operand between a and b: a must be greater than b
   */
  def sumMultipleParam(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sumMultipleParam(f)(a + 1, b)

  /**
   * a tail recursive mutiple parameter list method curried function for sum
   * @param f the mathematical operator
   * @param a the first input parameter
   * @param b the second parameter
   * @return sum of the operand between a and b: a must be greater than b
   */
  def sumMultParamTail(f: Int => Int)(a: Int, b: Int): Int ={
    def loop(a: Int, acc: Int): Int = if (a > b) acc else loop(a + 1, acc + f(a))
    loop(a, 0)
  }

  /**
   * a tail recursive mutiple parameter list method curried function for product
   * @param f the mathematical operator
   * @param a the first input parameter
   * @param b the second parameter
   * @return product of the operand between a and b: a must be greater than b
   */
  def productMultParam(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 1 else f(a) * productMultParam(f)(a + 1, b)
  def productMultParamTail(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = if (a > b) acc else loop(a + 1, acc * f(a))
    loop(a, 1)
  }
  def fact(x: Int): Int = productMultParamTail(x => x)(1, x)

  /**
   * generalized map reduce method
   * @param f operand
   * @param combine combinator
   * @param zero default value
   * @param a first parameter
   * @param b second parameter
   * @return
   */
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }
  /**
   * tail recursive generalized map reduce method
   * @param f operand
   * @param combine combinator
   * @param zero default value
   * @param a first parameter
   * @param b second parameter
   * @return
   */
  def mapReduceTail(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    def loop (a: Int, acc: Int): Int = if (a > b) acc else loop(a + 1, combine(f(a),acc))
    loop(a, zero)
  }


  def main(args:Array[String]){
    val time = System.currentTimeMillis()
    println(sumCurried(x => x*x)(2,6))
    println(sumMultipleParam(x => x*x)(2,6))
    println(sumMultParamTail(x => x*x)(2,6))
    println(productMultParam(x => x)(2,6))
    println(productMultParamTail(x => x)(2,6))
    println(fact(4))
    println(mapReduce(x => x, (x, y) => x*y, 1)(2,6))
    println(mapReduceTail(x => x, (x, y) => x * y, 1)(1,4))
//    println(System.currentTimeMillis() - time)
  }
}

