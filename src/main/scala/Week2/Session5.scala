package Week2
/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 5/14/14
 * Time: 5:00 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */
object Session5 {
  class Rational(x: Int, y: Int){
    require(y != 0, "denominator must be non zero")
    def this(x: Int) = this(x, 1)
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
    val g = gcd(x, y)
    val numer = x / g
    val denom = y / g
    def add(that: Rational):Rational={
      new Rational(
        this.numer * that.denom + this.denom * that.numer,
        this.denom * that.denom)
    }
    def toStringRational = "%s/%s".format(numer,denom)
    def neg = new Rational(-numer,denom)
    def sub(that: Rational) = this.add(that.neg)
    def less(that: Rational) = numer * that.denom < that.numer * denom
    def max(that: Rational) = if(this.less(that)) that else this
  }

  def main(args:Array[String]){
//    val x = new Rational(1,0)
    val x = new Rational(2,1)
    println(x.numer)
    println(x.denom)
    val y = new Rational(5,7)
    x.add(y)
    val z = new Rational(3,2)
    val result = x.add(y)
    println(result.toStringRational)
    println(x.sub(y).sub(z).toStringRational)
    println(y.add(y).toStringRational)
    println(x.max(y).toStringRational)
    println(x max y toStringRational)
  }
}
