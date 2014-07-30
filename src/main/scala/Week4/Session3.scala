package Week4

import math.Ordering
/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 7/27/14
 * Time: 1:43 AM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session3 {

  /**
   * Method to merge two list in pair case matching
   *
   * @param firstList
   * @param secondList
   * @return
   */
  def mergePairMatch[T](firstList: List[T], secondList: List[T])(comparatorFunc: (T,T)=> Boolean): List[T] = (firstList, secondList) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case (x :: xs, y :: ys) => {
      if (comparatorFunc(x, y)) x :: mergePairMatch(xs, secondList)(comparatorFunc)
      else y :: mergePairMatch(ys, firstList)(comparatorFunc)
    }
  }

  /**
   * Generic Method to merge and sort a list based on a input comparator function
   *
   * @param inputList
   * @param comparatorFunc
   * @tparam T
   * @return
   */
  def mergeSort[T](inputList: (List[T]))(comparatorFunc: (T,T)=> Boolean): List[T] = {

    def mergePairMatch(firstList: List[T], secondList: List[T])(comparatorFunc: (T,T)=> Boolean): List[T] =
      (firstList, secondList) match {
        case (Nil, y) => y
        case (x, Nil) => x
        case (x :: xs, y :: ys) => {
          if (comparatorFunc(x, y)) x :: mergePairMatch(xs, secondList)(comparatorFunc)
          else y :: mergePairMatch(ys, firstList)(comparatorFunc)
        }
    }

    val midPoint = inputList.length/2
    inputList match {
      case List() => inputList
      case List(x) => inputList
      case _ => {
        val (firstPart, secondPart) = inputList splitAt midPoint
        mergePairMatch(mergeSort(firstPart)(comparatorFunc), mergeSort(secondPart)(comparatorFunc))(comparatorFunc)
      }
    }
  }

  /**
   * Method to merge sort a list based on input Ordering function in this case inbuilt math.Ordering
   *
   * @param inputList
   * @param ord
   * @tparam T
   * @return
   */
  def mergeSortOrdering[T](inputList: (List[T]))(ord: Ordering[T]): List[T] = {

    def mergePairMatch(firstList: List[T], secondList: List[T]): List[T] =
      (firstList, secondList) match {
        case (Nil, y) => y
        case (x, Nil) => x
        case (x :: xs, y :: ys) => {
          if (ord.lt(x, y)) x :: mergePairMatch(xs, secondList)
          else y :: mergePairMatch(ys, firstList)
        }
      }

    val midPoint = inputList.length/2
    inputList match {
      case List() => inputList
      case List(x) => inputList
      case _ => {
        val (firstPart, secondPart) = inputList splitAt midPoint
        mergePairMatch(mergeSortOrdering(firstPart)(ord), mergeSortOrdering(secondPart)(ord))
      }
    }
  }

  /**
   * Method to merge sort a list using implicit parameter math.Ordering
   *
   * @param inputList
   * @param ord
   * @tparam T
   * @return
   */
  def mergeSortOrderingImplicit[T](inputList: (List[T]))(implicit ord: Ordering[T]): List[T] = {

    def mergePairMatch(firstList: List[T], secondList: List[T]): List[T] =
      (firstList, secondList) match {
        case (Nil, y) => y
        case (x, Nil) => x
        case (x :: xs, y :: ys) => {
          if (ord.lt(x, y)) x :: mergePairMatch(xs, secondList)
          else y :: mergePairMatch(ys, firstList)
        }
      }

    val midPoint = inputList.length/2
    inputList match {
      case List() => inputList
      case List(x) => inputList
      case _ => {
        val (firstPart, secondPart) = inputList splitAt midPoint
        mergePairMatch(mergeSortOrderingImplicit(firstPart), mergeSortOrderingImplicit(secondPart))
      }
    }
  }

  def main(args: Array[String]){
    println(mergeSort(List(2.4,1.0,8.5,9.3,2.7))((x: Double, y: Double) => x < y))
    println(mergeSort(List(2,1,8,9,2))((x: Int, y: Int) => x < y))
    println(mergeSort(List("mango", "apple", "banana"))((x: String, y: String) =>  x.compareTo(y) < 0))
    println(mergeSortOrdering(List(2,1,8,9,2))(Ordering.Int))
    println(mergeSortOrdering(List("mango", "apple", "banana"))(Ordering.String))
    println(mergeSortOrderingImplicit(List(2,1,8,9,2)))
    println(mergeSortOrderingImplicit(List("mango", "apple", "banana")))
  }
}
