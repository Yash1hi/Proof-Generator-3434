import org.scalatest.funsuite.AnyFunSuite
import Math.*

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()

  import generator._ // This imports all members from ProofGenerator instance



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
    println(premise)
    println(conclusion)

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
    println(theorems.last.getAntecedent)

    val startState = State(premise, null)
    val prover = Prover(theorems, premise, conclusion)
    val foundState = prover.bfs()
    println(foundState.getProof())

  }

  test("Example BFS Proof 3") {
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
    println(premise)

     val startState = State(premise, null)
     val prover = Prover(theorems, premise, conclusion)
     val foundState = prover.bfs()
     println(foundState.getProof())

  }

  
}

