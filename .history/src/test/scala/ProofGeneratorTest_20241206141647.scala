import 

class ProofGeneratorTest {
  // BEGIN TESTS
  // Kai simple
  val x0 = Prash
  for (x <- List(x0)) {
    assert(x.isInstanceOf[Kai], "test about Kai")
    assert(!x.isInstanceOf[Rahul], "test about Rahul")
    assert(!x.isInstanceOf[Shuchi], "test about Shuchi")
    assert(!x.isInstanceOf[Tome], "test about Tome")
  }
  // END TESTS
}
