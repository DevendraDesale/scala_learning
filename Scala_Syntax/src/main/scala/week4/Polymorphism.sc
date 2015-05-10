import week4._
object nth {
  def nth[T](n: Int, current: List[T]): T =
    if (n == 0) current.head
    else if (n > 0) nth(n - 1, current.tail)
    else throw new IndexOutOfBoundsException("No such element")

  val list = new Cons(1, new Cons(2, new Nil))

  nth(0, list)
}