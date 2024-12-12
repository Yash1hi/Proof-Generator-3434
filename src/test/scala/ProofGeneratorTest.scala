import org.scalatest.funsuite.AnyFunSuite
import Math.*

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()

  import generator._ // This imports all members from ProofGenerator instance

  test("Test Expression to String") {
    System.out.println(And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c"))))
    assert(And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c"))).toString() == "(a∧b→c)∧!c")
  }

//  test("Test Pass in Theorems") {
//    // De Morgan's Law
//    val t1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))))
//    // Transitive Property of And
//    val t2 = Theorem(And(Variable("a"), Variable("b")), And(Variable("b"), Variable("a")))
//    // Modus Tollens
//    val t4 = Theorem(Implies(Variable("a"), Variable("b")), Implies(Not(Variable("b")), Not(Variable("a"))))
//    // Implication Elimination
//    val t5 = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"))
//
//    System.out.println("antecedent = " + t5.getAntecedent)
//    System.out.println("consequent = " + t5.getConsequent)
//
//    val expr1 = And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c")))
//    val expr2 = And(Variable("c"), Variable("b"))
//    val expr3 = And(And(Variable("y"), Variable("z")), And(Variable("c"), Variable("d")))
//    val expr4 = And(Variable("a"), Implies(Variable("a"), Variable("b")))
//    //    System.out.println("expression = " + expr1)
//
//    val applicableExpressions: Seq[(Expr, Expr)] = t5.getApplicable(expr4)
//    applicableExpressions.foreach((e1: Expr, e2: Expr) => {
//      println("Theorem applied to: " + e1)
//      println("Result: " + e2)
//    })
//
//    val theorems = Vector(t1, t2, t4, t5)
//    println(theorems.last.getAntecedent)
//
//    val startState = State(expr1, null)
//    startState.expandState(theorems)
//    for (child1 <- startState.children) {
//      child1.expandState(theorems)
//      if (And(Not(Variable("c")), Implies(And(Variable("a"), Variable("b")), Variable("c"))) == child1.getExpr()) {
//        System.out.println("child1: " + child1.getExpr())
//        for (child2 <- child1.children) {
//          child2.expandState(theorems)
//          if (And(Not(Variable("c")), Implies(Not(Variable("c")), Not(And(Variable("a"), Variable("b"))))) == child2.getExpr()) {
//            System.out.println("child2: " + child2.getExpr())
//            for (child3 <- child2.children) {
//              child3.expandState(theorems)
//              System.out.println("child3: " + child3.getExpr())
//              for (child4 <- child3.children) {
//                child4.expandState(theorems).foreach((leaf: State) => {
//                  val up1 = leaf.getOperation().getPreviousState
//                  val up2 = up1.getOperation().getPreviousState
//                  val up3 = up2.getOperation().getPreviousState
//                  val up4 = up3.getOperation().getPreviousState
//                  val up5 = up4.getOperation().getPreviousState
//                  if (up2.getExpr() == Not(And(Variable("a"), Variable("b")))) {
//                    System.out.println("Leaf:" + up5.getExpr() + " -> " + up4.getExpr() + " -> " + up3.getExpr() + " -> " + up2.getExpr() + " -> " + up1.getExpr() + " -> " + leaf.getExpr())
//                  }
//                })
//              }
//            }
//          }
//        }
//      }
//    }
//  }

  test("Example BFS Proof") {
    // De Morgan's Law
    val t1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))), "De Morgan's Law")
    // Transitive Property of And
    val t2 = Theorem(And(Variable("a"), Variable("b")), And(Variable("b"), Variable("a")), "Transitive Property of And")
    // Modus Tollens
    val t4 = Theorem(Implies(Variable("a"), Variable("b")), Implies(Not(Variable("b")), Not(Variable("a"))), "Modus Tollens")
    // Implication Elimination
    val t5 = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"), "Implication Elimination")

    val premise = And(Implies(And(Variable("a"), Variable("b")), Variable("c")), Not(Variable("c")))
    val conclusion = Or(Not(Variable("b")), Not(Variable("a")))


    val theorems = Vector(t1, t2, t4, t5)
    println(theorems.last.getAntecedent)

    val startState = State(premise, null)
    val prover = Prover(theorems, premise, conclusion)
    val foundState = prover.bfs()
    println(foundState.getProof())

  }

  test("Cantor's Diagonal Argument") {
  }
}

