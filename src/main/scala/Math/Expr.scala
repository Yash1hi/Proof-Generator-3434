package Math

trait Expr {
  override def toString: String = this match {
    case Variable(n) => n
    case Not(e) => "!" + e
    case And(e1, e2) => "(" + e1 + "∧" + e2 + ")"
    case Or(e1, e2) => "(" + e1 + "∨" + e2 + ")"
    case Implies(e1, e2) => "(" + e1 + "→" + e2 + ")"
  }

  def uniqueVariables: Set[Variable] = this match {
    case Variable(n) => Set(Variable(n))
    case Not(e) => e.uniqueVariables
    case And(e1, e2) => e1.uniqueVariables ++ e2.uniqueVariables
    case Or(e1, e2) => e1.uniqueVariables ++ e2.uniqueVariables
    case Implies(e1, e2) => e1.uniqueVariables ++ e2.uniqueVariables
  }

  def findMatches(e: Expr, varMap: Map[Variable, Expr] = Map[Variable, Expr]()): Map[Variable, Expr] = {
    val vars = this.uniqueVariables
    var newMap = varMap

    (this, e) match {
      case (And(e1, e2), And(e3, e4)) => {
        (e1, e3) match {
          case (e1: Variable, e3) => {
            if (newMap.keySet.contains(e1)) {
              if (newMap.get(e1).get != e3) {
                return null;
              }
            } else {
              newMap = newMap + (e1 -> e3);
            }
          }
          case (e1, e3) => {
            val e1Match = e1.findMatches(e3, newMap)
            if (e1Match == null) {
              return null;
            }
            newMap = newMap ++ e1Match
          }
        }
        (e2, e4) match {
          case (e2: Variable, e4) => {
            if (newMap.keySet.contains(e2)) {
              if (newMap.get(e2).get != e4) {
                return null;
              }
            } else {
              newMap = newMap + (e2 -> e4);
            }
          }
          case (e2, e4) => {
            val e2Match = e2.findMatches(e4, newMap)
            if (e2Match == null) {
              return null;
            }
            newMap = newMap ++ e2Match
          }
        }
      }
      case (Or(e1, e2), Or(e3, e4)) => {
        (e1, e3) match {
          case (e1: Variable, e3) => {
            if (newMap.keySet.contains(e1)) {
              if (newMap.get(e1).get != e3) {
                return null;
              }
            } else {
              newMap = newMap + (e1 -> e3);
            }
          }
          case (e1, e3) => {
            val e1Match = e1.findMatches(e3, newMap)
            if (e1Match == null) {
              return null;
            }
            newMap = newMap ++ e1Match
          }
        }
        (e2, e4) match {
          case (e2: Variable, e4) => {
            if (newMap.keySet.contains(e2)) {
              if (newMap.get(e2).get != e4) {
                return null;
              }
            } else {
              newMap = newMap + (e2 -> e4);
            }
          }
          case (e2, e4) => {
            val e2Match = e2.findMatches(e4, newMap)
            if (e2Match == null) {
              return null;
            }
            newMap = newMap ++ e2Match
          }
        }
      }
      case (Implies(e1, e2), Implies(e3, e4)) => {
        (e1, e3) match {
          case (e1: Variable, e3) => {
            if (newMap.keySet.contains(e1)) {
              if (newMap.get(e1).get != e3) {
                return null;
              }
            } else {
              newMap = newMap + (e1 -> e3);
            }
          }
          case (e1, e3) => {
            val e1Match = e1.findMatches(e3, newMap)
            if (e1Match == null) {
              return null;
            }
            newMap = newMap ++ e1Match
          }
        }
        (e2, e4) match {
          case (e2: Variable, e4) => {
            if (newMap.keySet.contains(e2)) {
              if (newMap.get(e2).get != e4) {
                return null;
              }
            } else {
              newMap = newMap + (e2 -> e4);
            }
          }
          case (e2, e4) => {
            val e2Match = e2.findMatches(e4, newMap)
            if (e2Match == null) {
              return null;
            }
            newMap = newMap ++ e2Match
          }
        }
      }
      case (Not(e1), Not(e3)) => {
        (e1, e3) match {
          case (e1: Variable, e3) => {
            if (newMap.keySet.contains(e1)) {
              if (newMap.get(e1).get != e3) {
                return null;
              }
            } else {
              newMap = newMap + (e1 -> e3);
            }
          }
          case (e1, e3) => {
            val e1Match = e1.findMatches(e3, newMap)
            if (e1Match == null) {
              return null;
            }
            newMap = newMap ++ e1Match
          }
        }
      }
      case _ => {
        return null
      }
    }
    return newMap
  }

  def replaceVars(replacements: Map[Variable, Expr]): Expr = this match {
    case v@Variable(_) => replacements.getOrElse(v, v)
    case Not(e) => Not(e.replaceVars(replacements))
    case And(e1, e2) => And(e1.replaceVars(replacements), e2.replaceVars(replacements))
    case Or(e1, e2) => Or(e1.replaceVars(replacements), e2.replaceVars(replacements))
    case Implies(e1, e2) => Implies(e1.replaceVars(replacements), e2.replaceVars(replacements))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Expr =>
      (this, that) match {
        case (Variable(n1), Variable(n2)) => n1 == n2
        case (Not(e1), Not(e2)) => e1 == e2
        case (And(e1, e2), And(e3, e4)) => e1 == e3 && e2 == e4
        case (Or(e1, e2), Or(e3, e4)) => e1 == e3 && e2 == e4
        case (Implies(e1, e2), Implies(e3, e4)) => e1 == e3 && e2 == e4
        case _ => false
      }
    case _ => false
  }

  //  override def nequals(obj: Any): Boolean = {
  //    if (this == obj) {
  //      false
  //    } else {
  //      true
  //    }
  //  }

  override def hashCode: Int = this match {
    case Variable(n) => n.hashCode
    case Not(e) => 31 * e.hashCode
    case And(e1, e2) => 37 * e1.hashCode + 7 * e2.hashCode
    case Or(e1, e2) => 41 * e1.hashCode + 11 * e2.hashCode
    case Implies(e1, e2) => 101 * e1.hashCode + 13 * e2.hashCode
  }
}
case class Variable(n: String) extends Expr
case class Not(e: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Implies(e1: Expr, e2: Expr) extends Expr
