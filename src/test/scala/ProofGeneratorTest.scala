import org.scalatest.funsuite.AnyFunSuite
import Math.*

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()
  import generator._  // This imports all members from ProofGenerator instance
  
  test("Test Expression to String") {
    System.out.println(And(Parenthesis(Implies(And(Variable("a"), Variable("b")), Variable("c"))), Not(Variable("c"))))
    assert(And(Parenthesis(Implies(And(Variable("a"), Variable("b")), Variable("c"))), Not(Variable("c"))).toString() == "(a∧b→c)∧!c")
  }

  test("Test Pass in Theorems") {
    val t1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))))
    //    System.out.println(t1.getAntecedent())
    //    System.out.println(t1.getConsequent())

    val expr1 = And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c")))

    expr1 match {
      case And(e1, e2)  => {
        System.out.println(e1)
        System.out.println(e2)
      }
    }
  }


}
