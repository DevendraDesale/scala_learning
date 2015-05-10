/**
 * Creating the abstract classes
 * Implementing the binary tree
 */
abstract class IntSet{
  def incl(x: Int): IntSet
  def contains(x:Int): Boolean
  def union(other:IntSet): IntSet
}

/**
 * Scala you can get avoid creation of unique object
 * again and again by using the object as classes in java
 * provides more readability and perfect execution
 */
object Empty extends IntSet{
  def contains(x: Int): Boolean = false

  def incl(x:Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString = "."

  override def union(other:IntSet) = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true


  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{"+ left + elem + right + "}"

  override def union(other: IntSet) =
    ((left union right) union other) incl elem
}


val t1 = new NonEmpty(2, Empty, Empty)
val t2 = t1 incl 4


val t3 = new NonEmpty(5, Empty, Empty)
val t4 = t3 incl 3

t4 union t2

if (true)  1 else false