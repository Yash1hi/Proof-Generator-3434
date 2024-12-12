package Math

class Theorem(antecedent: Expr, consequent: Expr) {
  def getAntecedent: Expr = {
    antecedent
  }

  def getConsequent: Expr = {
    consequent
  }

  // returns (Expr Applied to, Expr with theorem applied)
  def getApplicable(e: Expr): Vector[(Expr, Expr)] = {
    var exprs = Vector.empty[(Expr, Expr)]

    val antecedentMatches = antecedent.findMatches(e)
    if (antecedentMatches != null) {
        exprs = exprs :+ (e, consequent.replaceVars(antecedentMatches))
    }

    e match {
      case And(e1, e2) => {
        getApplicable(e1).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, And(newExpr, e2))
        })
        getApplicable(e2).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, And(e1, newExpr))
        })
      }
      case Or(e1, e2) => {
        getApplicable(e1).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, Or(newExpr, e2))
        })
        getApplicable(e2).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, Or(e1, newExpr))
        })
      }
      case Implies(e1, e2) => {
        getApplicable(e1).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, Implies(newExpr, e2))
        })
        getApplicable(e2).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, Implies(e1, newExpr))
        })
      }
      case Not(e1) => {
        getApplicable(e1).foreach((appliedTo: Expr, newExpr: Expr) => {
          exprs = exprs :+ (appliedTo, Not(newExpr))
        })
      }
      case _ => {
        // Do nothing
      }
    }

    return exprs
  }
}
