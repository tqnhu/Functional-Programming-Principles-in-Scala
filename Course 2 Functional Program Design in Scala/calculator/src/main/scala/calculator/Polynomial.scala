package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {b()*b()-4*a()*c()}

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val negB = -1*b()
    val twoA = 2*a()
    val sqrtDel = math.sqrt(delta())

    if (delta() < 0) Set()
    else Set (
      (negB + sqrtDel)/twoA,
      (negB - sqrtDel)/twoA
    )
  }
}
