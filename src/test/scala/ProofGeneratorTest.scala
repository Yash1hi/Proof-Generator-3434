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
    // De Morgan's Law
    val t1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))))
    // Transitive Property of And
    val t2 = Theorem(And(Variable("a"), Variable("b")), And(Variable("b"), Variable("a")))
    // Not is distributive over And
    val t3 = Theorem(Not(And(Variable("a"), Variable("b"))), And(Not(Variable("a")), Not(Variable("b"))))
    // Modus Tollens
    val t4 = Theorem(Implies(Variable("a"), Variable("b")), Implies(Not(Variable("b")), Not(Variable("a"))))
    // Implication Elimination
    val t5 = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"))

    //    System.out.println("antecedent = " + t1.getAntecedent)
    //    System.out.println("consequent = " + t1.getConsequent)

    val expr1 = And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c")))
    val expr2 = And(Variable("c"), Variable("b"))
    val expr3 = And(And(Variable("y"), Variable("z")), And(Variable("c"), Variable("d")))

    //    System.out.println("expression = " + expr1)

    val applicableExpressions: Seq[(Expr, Expr)] = t2.getApplicable(expr1)
    applicableExpressions.foreach((e1: Expr, e2: Expr) => {
      println("Theorem applied to: " + e1)
      println("Result: " + e2)
    })

    val theorems = Vector(t1, t2, t3, t4, t5)

    val startState = State(expr1, null)
    startState.expandState(theorems)
    for (child1 <- startState.children) {
      child1.expandState(theorems)
      println()
      if (And(Not(Variable("c")), Implies(And(Variable("a"), Variable("b")), Variable("c"))) == child1.getExpr()) {
        for (child2 <- child1.children) {
          child2.expandState(theorems)
          for (child3 <- child2.children) {
            child3.expandState(theorems)
            for (child4 <- child3.children) {
              child4.expandState(theorems).foreach((leaf: State) => {
                val up1 = leaf.getOperation().getPreviousState
                val up2 = up1.getOperation().getPreviousState
                val up3 = up2.getOperation().getPreviousState
                val up4 = up3.getOperation().getPreviousState
                val up5 = up4.getOperation().getPreviousState
                //              if (up2.getExpr() == Not(And(Variable("a"), Variable("b")))) {
                //                System.out.println("Leaf:" + up5.getExpr() + " -> " + up4.getExpr() + " -> " + up3.getExpr() + " -> " + up2.getExpr() + " -> " + up1.getExpr() + " -> " + leaf.getExpr())
                //              }
              })
            }
          }
        }
      }
    }
  }
}

