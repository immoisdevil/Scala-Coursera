package Week2

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 5/14/14
 * Time: 11:22 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session7 {
  class Rational(x: Int, y: Int){
    require(y != 0, "denominator must be non zero")
    def this(x: Int) = this(x, 1)
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
    val g = gcd(x, y)
    val numer = x / g
    val denom = y / g
    def + (that: Rational):Rational={
      new Rational(
        this.numer * that.denom + this.denom * that.numer,
        this.denom * that.denom)
    }
    def toStringRational = "%s/%s".format(numer,denom)
    def unary_- = new Rational(-numer,denom)
    def binary_- (that: Rational) = this + -that
    def - (that: Rational) = this + -that
    def < (that: Rational) = numer * that.denom < that.numer * denom
    def max(that: Rational) = if(this < that) that else this
  }

  def main(args:Array[String]){
    //    val x = new Rational(1,0)
    val x = new Rational(2,1)
    println(x.numer)
    println(x.denom)
    val y = new Rational(5,7)
    x + y
    val z = new Rational(3,2)
    val result = x + y
    println(result toStringRational)
    println(x - y - z toStringRational)
    println(y + y toStringRational)
    println(x max y toStringRational)
    println(x max y toStringRational)
    println(x - y)
  }
}
