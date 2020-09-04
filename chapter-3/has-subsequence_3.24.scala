object subsequence {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(a: Int): Boolean = {
      if (a == sup.length - 1) false
      else if (sub.indexOf(sup(a)) > -1 && sup.slice(a, a + sub.length) == sub) true
      else loop(a + 1)
    }

    loop(0)
  }

  // provided answers which utilise tail recursion
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  
  @annotation.tailrec
  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5)
    val y = List(2, 3, 4)
    println(hasSubsequence(x, y))
  }
}
