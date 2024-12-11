package Math

trait Expr {
  override def toString: String = this match {
    case Variable(n) => n
    case Not(e) => "!" + e
    case And(e1, e2) => "(" + e1 + "∧" + e2 + ")"
    case Or(e1, e2) => "(" + e1 + "∨" + e2 + ")"
    case Implies(e1, e2) => "(" + e1 + "→" + e2 + ")"
    case Parenthesis(e1) => "(" + e1 + ")"
  }

  def uniqueVariables: Set[Variable] = this match {
    case Variable(n) => Set(Variable(n))
    case Not(e) => e.uniqueVariables
    case And(e1, e2) => e1.uniqueVariables ++ e2.uniqueVariables
    case Or(e1, e2) => e1.uniqueVariables ++ e2.uniqueVariables
    case Implies(e1, e2) => e1.uniqueVariables ++ e2.uniqueVariables
    case Parenthesis(e1) => e1.uniqueVariables
  }

  def findMatches(e: Expr, varMap: Map[Variable, Expr] = Map[Variable, Expr]()): Map[Variable, Expr] = {
    val vars = this.uniqueVariables
    var newMap = varMap

    (this, e) match {
      case (And(e1:Variable, e2:Variable), And(e3, e4)) => {
        if (newMap.keySet.contains(e1)) {
          if (newMap.get(e1) != e3) {
            return null;
          }
        } else {
          newMap = newMap + (e1 -> e3);
        }

        if (newMap.keySet.contains(e2)) {
          if (newMap.get(e2) != e4) {
            return null;
          }
        } else {
          newMap = newMap + (e2 -> e4);
        }
      }
      case (And(e1, e2), And(e3, e4)) => {
        val e1Match = e1.findMatches(e3, newMap)
        if (e1Match == null) {
          return null;
        }
        newMap = newMap ++ e1Match
        val e2Match = e1.findMatches(e3, newMap)
        if (e2Match == null) {
          return null;
        }
        newMap = newMap ++ e2Match
      }
      
      case (Or(e1:Variable, e2:Variable), Or(e3, e4)) => {
        if (newMap.keySet.contains(e1)) {
          if (newMap.get(e1) != e3) {
            return null;
          }
        } else {
          newMap = newMap + (e1 -> e3);
        }

        if (newMap.keySet.contains(e2)) {
          if (newMap.get(e2) != e4) {
            return null;
          }
        } else {
          newMap = newMap + (e2 -> e4);
        }
      }
      case (Or(e1, e2), Or(e3, e4)) => {
        val e1Match = e1.findMatches(e3, newMap)
        if (e1Match == null) {
          return null;
        }
        newMap = newMap ++ e1Match
        val e2Match = e1.findMatches(e3, newMap)
        if (e2Match == null) {
          return null;
        }
        newMap = newMap ++ e2Match
      }
      
      case (Implies(e1:Variable, e2:Variable), Implies(e3, e4)) => {
        if (newMap.keySet.contains(e1)) {
          if (newMap.get(e1) != e3) {
            return null;
          }
        } else {
          newMap = newMap + (e1 -> e3);
        }

        if (newMap.keySet.contains(e2)) {
          if (newMap.get(e2) != e4) {
            return null;
          }
        } else {
          newMap = newMap + (e2 -> e4);
        }
      }
      case (Implies(e1, e2), Implies(e3, e4)) => {
        val e1Match = e1.findMatches(e3, newMap)
        if (e1Match == null) {
          return null;
        }
        newMap = newMap ++ e1Match
        val e2Match = e1.findMatches(e3, newMap)
        if (e2Match == null) {
          return null;
        }
        newMap = newMap ++ e2Match
      }

      case (Not(e1:Variable), Not(e3)) => {
        if (newMap.keySet.contains(e1)) {
          if (newMap.get(e1) != e3) {
            return null;
          }
        } else {
          newMap = newMap + (e1 -> e3);
        }
      }
      case (Not(e1), Not(e3)) => {
        val e1Match = e1.findMatches(e3, newMap)
        if (e1Match == null) {
          return null;
        }
        newMap = newMap ++ e1Match
      }
      
      case _ => {
        return null
      }
    }
    return newMap
  }

  def replaceVars(replacements: Map[Variable, Expr]): Expr = this match {
    case v @ Variable(_) => replacements.getOrElse(v, v)
    case Not(e) => Not(e.replaceVars(replacements))
    case And(e1, e2) => And(e1.replaceVars(replacements), e2.replaceVars(replacements))
    case Or(e1, e2) => Or(e1.replaceVars(replacements), e2.replaceVars(replacements))
    case Implies(e1, e2) => Implies(e1.replaceVars(replacements), e2.replaceVars(replacements))
    case Parenthesis(e1) => Parenthesis(e1.replaceVars(replacements))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Expr =>
      (this, that) match {
        case (Variable(n1), Variable(n2)) => n1 == n2
        case (Not(e1), Not(e2)) => e1 == e2
        case (And(e1, e2), And(e3, e4)) => e1 == e3 && e2 == e4
        case (Or(e1, e2), Or(e3, e4)) => e1 == e3 && e2 == e4
        case (Implies(e1, e2), Implies(e3, e4)) => e1 == e3 && e2 == e4
        case (Parenthesis(e1), Parenthesis(e2)) => e1 == e2
        case _ => false
      }
    case _ => false
  }

  override def hashCode: Int = this match {
    case Variable(n) => n.hashCode
    case Not(e) => 31 * e.hashCode
    case And(e1, e2) => 31 * e1.hashCode + e2.hashCode
    case Or(e1, e2) => 31 * e1.hashCode + e2.hashCode
    case Implies(e1, e2) => 31 * e1.hashCode + e2.hashCode
    case Parenthesis(e1) => e1.hashCode
  }
}

case class Variable(n: String) extends Expr
case class Not(e: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Implies(e1: Expr, e2: Expr) extends Expr
case class Parenthesis(e1: Expr) extends Expr
