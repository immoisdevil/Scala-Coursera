package Week2

import Week1.Session7.tailFactorial

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 4/8/14
 * Time: 1:18 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session1 {
  class LowerOderFunctions{
    /**
     * Method to get sum of all the integers between a and b: a must be smaller than b
     * @param a First parameter
     * @param b Second parameter
     * @return Sum of all the integers between a and b
     */
    def sumInts(a :Int, b: Int): Int = if (a > b) 0 else a + sumInts(a + 1, b)

    /**
     * Method to get sum of all the integers between a and b
     * @param a First parameter
     * @param b Second parameter
     * @return Sum of all the integers between a and b
     */
    def sumIntsAll(a :Int, b: Int): Int = if (a > b) sumInts(b, a) else sumInts(a, b)

    /**
     * Method to get cube of a number
     * @param x input parameter
     * @return
     */
    def cube(x:Int) = x*x*x

    /**
     * Method to get sum of all cubes between a and b: a must be smaller than b
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def sumCubes(a: Int, b: Int): Int = if (a > b) 0 else cube(a) + sumCubes(a+1,b)

    /**
     * Method to get sum of all cubes between a and b
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def sumCubesAll(a: Int, b: Int): Int = if (a > b) sumCubes(b, a) else sumCubes(a, b)

    /**
     * Method to get sum of all factorials between a and b: a must be smaller than b
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def sumFacts(a: Int, b: Int ): Int = if (a > b) 0 else tailFactorial(a) + sumFacts(a+1 , b)
    /**
     * Method to get sum of all factorials between a and b
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def sumFactsAll(a: Int, b: Int): Int = if (a > b) sumFacts(b, a) else sumFacts(a, b)


  }

  class HigherOrderFunctions{
    val lower = new LowerOderFunctions()

    /**
     * Method to get sum of all the output values defined by function operator f between a and b: a must be smaller than b
     * @param f the mathematical operator eg. cube , factorial
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def sum(f: Int => Int, a: Int, b: Int): Int = if (a > b) 0 else f(a) + sum(f, a + 1, b)

    /**
     * Method to get sum of all the output values defined by function operator f between a and b
     * @param f the mathematical operator eg. cube , factorial
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def sumAll(f: Int => Int, a: Int, b: Int): Int = if (a > b) sum(f, b, a) else sum(f, a , b)

    def sumInts(a: Int, b: Int) = sumAll((x:Int) => x, a, b)
    def sumCubes(a: Int, b: Int) = sumAll(lower.cube, a, b)
    def sumfacts(a: Int, b: Int) = sumAll(tailFactorial, a, b)

    /**
     * Method to tail recursively get sum of all the output values defined by function operator f between a and b: a must be smaller than b
     * @param f the mathematical operator eg. cube , factorial
     * @param a first parameter
     * @param b second parameter
     * @return
     */
    def tailSum(f: Int => Int, a: Int, b: Int): Int ={
      def loop(a: Int, acc: Int): Int ={
        if (a > b) acc
        else loop(a + 1, f(a) + acc)
      }
      loop(a , 0)
    }
  }
  def main(args:Array[String]){
    val x = new LowerOderFunctions()
    println(x.sumCubes(2,6))
    println(x.sumCubes(6,2))
    println(x.sumCubesAll(2,6))
    println(x.sumCubesAll(6,2))
    val y = new HigherOrderFunctions()
    println(y.sumInts(2,6))
    println(y.sumCubes(2,6))
    println(y.sumfacts(2,6))
    println(y.tailSum((x) => x*x,2,6))
  }
}
