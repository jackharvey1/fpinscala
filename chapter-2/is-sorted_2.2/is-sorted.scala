object isSorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false

    loop(0)
  }

  def numericallyOrdered(a: Int, b: Int): Boolean =
    if (a > b) false
    else true

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 2, 3), numericallyOrdered))
    println(isSorted(Array(1, 2, 3, 2), numericallyOrdered))
    println(isSorted(Array(2, 2, 3), numericallyOrdered))
  }
}
