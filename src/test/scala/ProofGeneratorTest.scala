import org.scalatest.funsuite.AnyFunSuite
import Math.*

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()

  test("Theorem Examples") {
    // Modus Ponens
    val modus1 = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"), "Modus Ponens")
    // Modus Tollens
    val modus2 = Theorem(Implies(Variable("a"), Variable("b")), Implies(Not(Variable("b")), Not(Variable("a"))), "Modus Tollens")

    // De Morgan's Laws
    val deMorgans1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))), "De Morgan's Law", true)
    val deMorgans2 = Theorem(Or(Not(Variable("a")), Not(Variable("b"))), Not(And(Variable("a"), Variable("b"))), "De Morgan's Law", true)

    // Transitive Property (And/Or)
    val trans1 = Theorem(Or(Variable("a"), Variable("b")), Or(Variable("b"), Variable("a")), "Transitive Property of Or")
    val trans2 = Theorem(And(Variable("a"), Variable("b")), And(Variable("b"), Variable("a")), "Transitive Property of And")

    // Distributive Property (And/Or)
    val distAndOverOr = Theorem(And(Variable("a"), Or(Variable("b"), Variable("c"))), Or(And(Variable("a"), Variable("b")), And(Variable("a"), Variable("c"))), "Distributive Property", true)
    val distOrOverAnd = Theorem(Or(Variable("a"), And(Variable("b"), Variable("c"))), And(And(Variable("a"), Variable("b")), Or(Variable("a"), Variable("c"))), "Distributive Property", true)
    
    // Implication Elimination
    val impElim = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"), "Implication Elimination")

    // Double Negation
    val doubleNeg = Theorem(Not(Not(Variable("a"))), Variable("a"), "Double Negation", true)


    var theorems = Vector(modus1, modus2, deMorgans1, deMorgans2, trans1, trans2)
    theorems = theorems ++ Vector(distAndOverOr, distOrOverAnd, impElim, doubleNeg)

    theorems.foreach((t: Theorem) => {
      println(t)
    })
  }
  
  test("Some Swing Both Ways") {
    val doubleNeg = Theorem(Not(Not(Variable("a"))), Variable("a"), "Double Negation", true)
    println(doubleNeg.getApplicable(Variable("a")))
    println(doubleNeg.getApplicable(Not(Not(Variable("a")))))
  }

  test("Example BFS Proof 1") {
    // Modus Ponens
    val t1 = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"), "Modus Ponens")
    // Transitive Property of Or
    val t2 = Theorem(Or(Variable("a"), Variable("b")), Or(Variable("b"), Variable("a")), "Transitive Property of Or")
    // Distributive Property
    val t3 = Theorem(And(Variable("a"), Or(Variable("b"), Variable("c"))), Or(And(Variable("a"), Variable("b")), And(Variable("a"), Variable("c"))), "Distributive Property")
    // Double Negation
    val t4 = Theorem(Not(Not(Variable("a"))), Variable("a"), "Double Negation")

    val premise = And(Variable("a"), Implies(Variable("a"), Not(Not(Variable("p"))))) 
    val conclusion = Variable("p")

    val theorems = Vector(t1, t2, t3, t4)
//    println(premise)
//    println(conclusion)

    val startState = State(premise, null)
    val prover = Prover(theorems, premise, conclusion)
    val foundState = prover.bfs()
    println(foundState.getProof())

  }

  test("Example BFS Proof 2") {
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
//    println(theorems.last.getAntecedent)

    val startState = State(premise, null)
    val prover = Prover(theorems, premise, conclusion)
    val foundState = prover.bfs()
    println(foundState.getProof())

  }

  test("Example Proof: Use an Umbrella") {
    /*
     * If it is cloudy or rainy, use an umbrella.
     * It is not true that it is rainy nor cloudy.
     * Should we use an umbrella?
     */
    println("GIVEN: ")
    println("   If it is cloudy or rainy, use an umbrella.")
    println("   AND")
    println("   It is not true that it is either not rainy or not cloudy.")
    println("PROVE: ")
    println("   Should we use an umbrella?")


    // De Morgan's Law
    val t1 = Theorem(Not(And(Variable("a"), Variable("b"))), Or(Not(Variable("a")), Not(Variable("b"))), "De Morgan's Law")
    val t3 = Theorem(Or(Not(Variable("a")), Not(Variable("b"))), Not(And(Variable("a"), Variable("b"))), "De Morgan's Law")
    // Transitive Property of And
    val t2 = Theorem(And(Variable("a"), Variable("b")), And(Variable("b"), Variable("a")), "Transitive Property of And")
    // Modus Tollens
    val t4 = Theorem(Implies(Variable("a"), Variable("b")), Implies(Not(Variable("b")), Not(Variable("a"))), "Modus Tollens")
    // Implication Elimination
    val t5 = Theorem(And(Variable("a"), Implies(Variable("a"), Variable("b"))), Variable("b"), "Implication Elimination")
    val t6 = Theorem(Not(Not(Variable("a"))), Variable("a"), "Double Negation")

    val premise = And(Implies(And(Variable("Cloudy"), Variable("Rainy")), Variable("Umbrella")), Not(Or(Not(Variable("Rainy")), Not(Variable("Cloudy")))))
    val conclusion = Variable("Umbrella")


    val theorems = Vector(t1, t2, t3, t4, t5, t6)
//    println(premise)

     val startState = State(premise, null)
     val prover = Prover(theorems, premise, conclusion)
     val foundState = prover.bfs()
     println(foundState.getProof())

  }

  
}

