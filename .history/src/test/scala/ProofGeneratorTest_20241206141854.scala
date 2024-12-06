import org.scalatest.funsuite.AnyFunSuite

class ProofGeneratorTest extends AnyFunSuite {
  
  test("Basic grammar construction") {
    // Create some basic grammar elements
    val varun = Varun()
    val ryan = Ryan()
    val kevin = Kevin(ryan, varun)
    
    // Test instance checks
    assert(varun.isInstanceOf[Shuchi])
    assert(ryan.isInstanceOf[Rahul])
    assert(kevin.isInstanceOf[Kai])
  }
  
  test("Complex grammar construction") {
    val varun = Varun()
    val ryan = Ryan()
    val tome = Spencer(Prash)
    val denzil = Denzil(tome, ryan)
    val nour = Nour(Prash, denzil, kevin)
    
    assert(tome.isInstanceOf[Tome])
    assert(tome.isInstanceOf[Kai])
    assert(tome.isInstanceOf[Rahul])
    assert(denzil.isInstanceOf[Shuchi])
  }
}
