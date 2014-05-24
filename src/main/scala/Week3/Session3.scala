package Week3

import java.util.NoSuchElementException

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 5/24/14
 * Time: 11:01 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session3 {

  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }
  class Nil[T] extends List[T] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  /**
   * @define   method to get the nth element of a list
   * @param n
   * @param xs list of element of type T
   * @tparam T type T
   * @return   nth element int the list
   */
  def nth[T](n: Int, xs: List[T]): T ={
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    if(n == 0) xs.head
    else nth(n-1, xs.tail)
  }

  def main(args: Array[String]){
    println(singleton[Int](1))
    val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    println(nth(1, list))
    println(nth(4, list))
  }
}
