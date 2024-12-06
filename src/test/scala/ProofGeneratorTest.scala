import org.scalatest.funsuite.AnyFunSuite

class ProofGeneratorTest extends AnyFunSuite {
  val generator = new ProofGenerator()
  import generator._  // This imports all members from ProofGenerator instance
  
  test("Basic grammar construction") {
    assert(eval(Const(1)) == InvalidOdd)
    assert(eval(Const(1.5)) == NumValue(1.5))
    assert(eval(Const(2)) == NumValue(2))
    // Neg Tests
    assert(eval(Neg(Const(2))) == NumValue(-2))
    assert(eval(Neg(Const(1))) == InvalidOdd)
    // Add Tests
    assert(eval(Plus(Const(2), Const(4))) == NumValue(6))
    assert(eval(Plus(Const(1), Const(4))) == InvalidOdd)
    assert(eval(Plus(Const(12), Const(5))) == InvalidOdd)
    //
    eval(Minus(Mult(Const(65), Const(2)), Const(61)))
  }
}
