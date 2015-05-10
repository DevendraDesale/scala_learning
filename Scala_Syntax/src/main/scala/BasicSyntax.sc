
val x = new Rational(1, 2)
val y = new Rational(1,3)
val z = new Rational(5,7)

x - y

x.numer


x + new Rational(2,4)
x < y
x.max(y)


class Rational(x: Int, y: Int){

  require(y != 0, "Denominator must be non-zero")

  /**
   * Define new constructor with x
   * @param x
   * @return Rational object
   */
  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a%b)

  val numer = x
  val denom = y

  /**
   * Scala can use operator overloading
   * @param that
   * @return
   */
  def < (that:Rational) = numer * that.denom < that.numer * denom

  def max(that : Rational) = if(this < that) that else this

  override def toString() = {
     val g = gcd(x,y)
    numer / g + "/" + denom / g
  }


  def + (that:Rational): Rational=
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  /**
   * Prefix operator have different syntax
   * use unary for getting the prefix operator
   * Remember to give space between method name and :
   * as : could be part of method name
   * @return
   */
  def unary_- : Rational = new Rational(-numer,denom)

  def - (that:Rational):Rational =
    this + -that
}


import week4._

def nth[T](n: Int,current: List[T]): T =
  if (current isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) current.head
  else if (n > 0) nth(n-1, current.tail)
  else throw new IndexOutOfBoundsException

val list = new Cons1(2, new Nil1)

nth(0, list)