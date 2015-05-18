package chap3

object Exe26 extends App {
  assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 3)
}
