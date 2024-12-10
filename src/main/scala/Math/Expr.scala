package Math

trait Expr {
  override def toString: String = this match {
    case Variable(n) => n
    case Not(e) => "!" + e
    case And(e1, e2) => "" + e1 + "∧" + e2
    case Or(e1, e2) => "" + e1 + "∨" + e2
    case Implies(e1, e2) => "" + e1 + "→" + e2
    case Parenthesis(e1) => "(" + e1 + ")"
  }
}

case class Variable(n: String) extends Expr
case class Not(e: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Implies(e1: Expr, e2: Expr) extends Expr
case class Parenthesis(e1: Expr) extends Expr
