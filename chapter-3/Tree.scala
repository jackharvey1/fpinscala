sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def main(args: Array[String]): Unit = {
    println(size(Branch(Leaf(2), Leaf(3))), 3)
    println(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))), 5)
    println(size(Branch(Leaf(3), Branch(Leaf(1), Leaf(2)))), 5)
  }
}
