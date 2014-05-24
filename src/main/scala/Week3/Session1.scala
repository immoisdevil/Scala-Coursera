package Week3

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 5/16/14
 * Time: 9:34 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session1 {
  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }
  class Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
    def union(other: IntSet): IntSet = other
    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem ) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem
    override def toString = "{" + left + elem + right + "}"
  }
  def main(args: Array[String]){
    val t1 = new NonEmpty(3, new Empty, new Empty)
    val t2 = t1 incl 4
    println(t2 toString)
  }
}
