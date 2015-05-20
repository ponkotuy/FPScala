package chap3

object Exe29 extends App {
  def size(as: Tree[_]): Int = Tree.fold(as)(_ => 1)(_ + _ + 1)
  def maximum(as: Tree[Int]): Int = Tree.fold(as)(identity)(_ max _)
  def depth(as: Tree[_]): Int = Tree.fold(as)(_ => 1) { (l, r) => (l max r) + 1 }
  def map[A, B](as: Tree[A])(f: A => B): Tree[B] =
    Tree.fold[A, Tree[B]](as) { x => Leaf(f(x)) } { (l, r) => Branch(l, r) }

  val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  assert(size(tree) == 7)
  assert(maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 3)
  assert(depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 3)
  val before = Branch(Branch(Leaf(1), Leaf(2)), Leaf(4))
  val after = Branch(Branch(Leaf(2), Leaf(3)), Leaf(5))
  assert(map(before)(_ + 1) == after)
}
