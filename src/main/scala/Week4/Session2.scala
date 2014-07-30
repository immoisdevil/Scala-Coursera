package Week4

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 7/8/14
 * Time: 7:40 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session2 {

  /**
   * Method to merge two list
   *
   * @param firstList
   * @param secondList
   * @return
   */
  def merge(firstList: List[Double], secondList: List[Double]): List[Double] = {
    firstList match {
      case List() => secondList
      case x :: xs => {
        secondList match {
          case List() => firstList
          case y :: ys => {
            if (x > y) x :: merge(xs, secondList)
            else y :: merge(ys, firstList)
          }
        }
      }
    }
  }

  /**
   * Method to merge two list in pair case matching
   *
   * @param firstList
   * @param secondList
   * @return
   */
  def mergePairMatch(firstList: List[Double], secondList: List[Double]): List[Double] = (firstList, secondList) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case (x :: xs, y :: ys) => {
      if (x > y) x :: mergePairMatch(xs, secondList)
      else y :: mergePairMatch(ys, firstList)
    }
  }

  /**
   * Method to merge and sort a list in descending order
   *
   * @param inputList
   * @return
   */
  def mergeSort(inputList: (List[Double]), mergeFunc: (List[Double], List[Double]) => List[Double]): List[Double] = {

    val midPoint = inputList.length/2
    inputList match {
      case List() => inputList
      case List(x) => inputList
      case _ => {
        val (firstPart, secondPart) = inputList splitAt midPoint
        mergeFunc(mergeSort(firstPart, mergeFunc), mergeSort(secondPart, mergeFunc))
      }
    }
  }

  def main(args: Array[String]){
    println(mergeSort(List(2.4,1.0,8.5,9.3,2.7), merge _))
    println(mergeSort(List(2.4,1.0,8.5,9.3,2.7), mergePairMatch _))
  }
}
