sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(x) => x
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(x) => 1
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(x) => Leaf(f(x))
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(x) => f(x) 
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((d1, d2) => 1 + (d1 max d2))

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def main(args: Array[String]): Unit = {
    val x = Branch(
      Branch(
        Leaf(9), 
        Branch(
          Leaf(11),
          Leaf(65) 
        ), 
      ),
      Branch(
        Leaf(2), 
        Leaf(4)
      )
    )
    println(size2(x))
    println(maximum2(x))
    println(depth2(x))
    println(map2(x)(_ * 2))
  }
}
