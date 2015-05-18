package chap3

object Exe27 extends App {
  assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 3)
}
