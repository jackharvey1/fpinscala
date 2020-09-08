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

  def main(args: Array[String]): Unit = {
    println(
      map(
        Branch(
          Branch(
            Leaf(9), 
            Branch(
              Leaf(11),
              Leaf(65) // 4 depth
            ), 
          ),
          Branch(
            Leaf(2), 
            Leaf(4)
          )
        )
      )(_ * 2)
    )
  }
}
