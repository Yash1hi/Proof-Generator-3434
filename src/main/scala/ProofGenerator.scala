class ProofGenerator {
  // _YOUR_CODE_HERE_
  sealed trait Expr
  case class Const(n: Double) extends Expr
  case class Neg(e: Expr) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Mult(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class TryCatch(e1: Expr, e2: Expr) extends Expr

  sealed trait Value
  case class NumValue(n: Double) extends Value

  sealed trait ErrorValue extends Value
  case object InvalidDivByZero extends ErrorValue
  case object InvalidOdd extends ErrorValue

  // _YOUR_CODE_HERE_
  def eval(e: Expr): Value = {
    e match {
      case Const(n) => {
        if (n % 2 == 1) InvalidOdd
        else NumValue(n)
      }
      case Neg(e) => {
        eval(e) match {
          case NumValue(n1) => NumValue(-n1)
          case err => err
        }
      }
      case Plus(e1, e2) => {
        (eval(e1), eval(e2)) match {
          case (NumValue(n1), NumValue(n2)) => NumValue(n1 + n2)
          case (err, NumValue(n1)) => err
          case (NumValue(n1), err) => err
          case (err, _) => err
        }
      }
      case Minus(e1, e2) => {
        (eval(e1), eval(e2)) match {
          case (NumValue(n1), NumValue(n2)) => NumValue(n1 - n2)
          case (NumValue(n1), err) => err
          case (err, NumValue(n1)) => err
          case (err, _) => err
        }
      }
      case Mult(e1, e2) => {
        (eval(e1), eval(e2)) match {
          case (NumValue(n1), NumValue(n2)) => NumValue(n1 * n2)
          case (err, NumValue(n1)) => err
          case (NumValue(n1), err) => err
          case (err, _) => err
        }
      }
      case Div(e1, e2) => {
        (eval(e1), eval(e2)) match {
          case (NumValue(n1), NumValue(0)) => InvalidDivByZero
          case (NumValue(n1), NumValue(n2)) => NumValue(n1 / n2)
          case (err, NumValue(n2)) => err
          case (NumValue(n1), err) => err
          case (err, _) => err
        }
      }
      case TryCatch(e1, e2) => {
        (eval(e1), eval(e2)) match {
          case (NumValue(n1), _) => NumValue(n1)
          case (_, NumValue(n2)) => NumValue(n2)
          case (_, err) => err
        }
      }
    }
  }


}
