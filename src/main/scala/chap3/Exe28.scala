package chap3

object Exe28 extends App {
  val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(4))
  val result = Branch(Branch(Leaf(2), Leaf(3)), Leaf(5))
  assert(Tree.map(tree)(_ + 1) == result)
}
