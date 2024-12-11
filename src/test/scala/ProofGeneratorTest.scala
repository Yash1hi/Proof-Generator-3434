import org.scalatest.funsuite.AnyFunSuite
import Math.*

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()

  import generator._ // This imports all members from ProofGenerator instance

  test("Test Expression to String") {
    System.out.println(And(Parenthesis(Implies(And(Variable("a"), Variable("b")), Variable("c"))), Not(Variable("c"))))
    assert(And(Parenthesis(Implies(And(Variable("a"), Variable("b")), Variable("c"))), Not(Variable("c"))).toString() == "(a∧b→c)∧!c")
  }

  test("Test Pass in Theorems") {
    val t1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))))
    val t2 = Theorem(And(Variable("a"), Variable("b")), And(Variable("b"), Variable("a")))
    System.out.println("antecedent = " + t1.getAntecedent)
    System.out.println("consequent = " + t1.getConsequent)

    val expr1 = And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c")))
    val expr2 = And(Variable("c"), Variable("b"))
    val expr3 = And(And(Variable("y"), Variable("z")), And(Variable("c"), Variable("d")))

    System.out.println("expression = " + expr1)

    val applicableExpressions: Seq[Expr] = t2.getApplicable(expr1)
    applicableExpressions.foreach((e: Expr) => println(e))

  }
}

