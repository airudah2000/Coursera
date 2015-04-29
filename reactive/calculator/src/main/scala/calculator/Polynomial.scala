package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = Signal{
    math.pow(b(), 2) - ((4 * a()) * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal{
    val deltaRoot = math.sqrt(delta())
    val twoA = 2 * a.apply
    if(delta.apply < 0)
      Set()
    else{
      val setA = (-b.apply + deltaRoot)/twoA
      val setB = (-b.apply - deltaRoot)/twoA
      Set(setA, setB)
    }
  }
}
