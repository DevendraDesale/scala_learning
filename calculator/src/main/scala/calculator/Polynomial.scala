package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      val aVal = a()
      val bVal = b()
      val cVal = c()
      bVal * bVal - 4 * aVal * cVal
    }
  }

  /*(-b ± √Δ) / (2a)*/
  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val aVal = a()
      val bVal = b()
      val dVal = delta()
      Set(-bVal + Math.sqrt(dVal)/ (2 * aVal), -bVal - Math.sqrt(dVal)/ (2 * aVal))
    }
  }
}
