{
  class AnonFun extends Function1[Int, Int] {
    def apply(x: Int) = x * x
  }
  val f = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }

  f.apply(4)
}