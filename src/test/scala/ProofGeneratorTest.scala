import org.scalatest.funsuite.AnyFunSuite

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()
  import generator._  // This imports all members from ProofGenerator instance
  
  test("Test Expression to String") {
    System.out.println(And(Parenthesis(Implies(And(Variable("a"), Variable("b")), Variable("c"))), Not(Variable("c"))))
    assert(And(Parenthesis(Implies(And(Variable("a"), Variable("b")), Variable("c"))), Not(Variable("c"))).toString() == "(a∧b→c)∧!c")
  }
}
