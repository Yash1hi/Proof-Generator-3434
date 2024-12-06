class ProofGenerator {
  sealed trait Expr {
    override def toString: String = this match {
      case Variable(n)          => n
      case Not(e)               => "!$e"
      case And(e1, e2)          => "(" + e1 + "∧" + e2 + ")"
      case Or(e1, e2)           => "($e1 ∨ $e2)"
      case Implies(e1, e2)      => "($e1 → $e2)"
      case Parenthesis(e1)      => "($e1)"
    }
  }

  case class Variable(n: String) extends Expr
  case class Not(e: Expr) extends Expr
  case class And(e1: Expr, e2: Expr) extends Expr
  case class Or(e1: Expr, e2: Expr) extends Expr
  case class Implies(e1: Expr, e2: Expr) extends Expr
  case class Parenthesis(e1: Expr) extends Expr

  sealed trait Value {
    override def toString: String = this match {
      case Boolean(n) => n
    }
  }
  case class Boolean(n: String) extends Value

  sealed trait ErrorValue extends Value
  case object InvalidDivByZero extends ErrorValue
  case object InvalidOdd extends ErrorValue



//  def eval(e: Expr): Value = {
//    e match {
//      case Variable(n) => {
//        Boolean(n)
//      }
//      case Not(e) => {
//        eval(e) match {
//          case NumValue(n1) => NumValue(-n1)
//          case err => err
//        }
//      }
//      case Plus(e1, e2) => {
//        (eval(e1), eval(e2)) match {
//          case (NumValue(n1), NumValue(n2)) => NumValue(n1 + n2)
//          case (err, NumValue(n1)) => err
//          case (NumValue(n1), err) => err
//          case (err, _) => err
//        }
//      }
//      case Minus(e1, e2) => {
//        (eval(e1), eval(e2)) match {
//          case (NumValue(n1), NumValue(n2)) => NumValue(n1 - n2)
//          case (NumValue(n1), err) => err
//          case (err, NumValue(n1)) => err
//          case (err, _) => err
//        }
//      }
//      case Mult(e1, e2) => {
//        (eval(e1), eval(e2)) match {
//          case (NumValue(n1), NumValue(n2)) => NumValue(n1 * n2)
//          case (err, NumValue(n1)) => err
//          case (NumValue(n1), err) => err
//          case (err, _) => err
//        }
//      }
//      case Div(e1, e2) => {
//        (eval(e1), eval(e2)) match {
//          case (NumValue(n1), NumValue(0)) => InvalidDivByZero
//          case (NumValue(n1), NumValue(n2)) => NumValue(n1 / n2)
//          case (err, NumValue(n2)) => err
//          case (NumValue(n1), err) => err
//          case (err, _) => err
//        }
//      }
//      case TryCatch(e1, e2) => {
//        (eval(e1), eval(e2)) match {
//          case (NumValue(n1), _) => NumValue(n1)
//          case (_, NumValue(n2)) => NumValue(n2)
//          case (_, err) => err
//        }
//      }
//    }
//  }


}
