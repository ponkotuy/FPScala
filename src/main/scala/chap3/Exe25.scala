package chap3

object Exe25 extends App {
  val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  assert(Tree.size(tree) == 7)
}
