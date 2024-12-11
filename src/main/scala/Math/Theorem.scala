package Math

class Theorem(antecedent: Expr, consequent: Expr) {
  def getAntecedent: Expr = {
    antecedent
  }

  def getConsequent: Expr = {
    consequent
  }

  def getApplicable(e: Expr): Vector[Expr] = {
    var exprs = Vector.empty[Expr]

//    val antecedentVars = antecedent.uniqueVariables
//    val eVars = e.uniqueVariables
//    if (antecedentVars.size == eVars.size) {
//      val varMap = antecedentVars.toList.zip(eVars.toList).toMap
//      if (antecedent.replaceVars(varMap) == e) {
//        exprs = exprs :+ e
//      }
//    }
    val antecedentMatches = antecedent.findMatches(e)
    if (antecedentMatches != null) {
        exprs = exprs :+ e
    }

    e match {
      case And(e1, e2) => {
        exprs = exprs ++ getApplicable(e1)
        exprs = exprs ++ getApplicable(e2)
      }
      case Or(e1, e2) => {
        exprs = exprs ++ getApplicable(e1)
        exprs = exprs ++ getApplicable(e2)
      }
      case Implies(e1, e2) => {
        exprs = exprs ++ getApplicable(e1)
        exprs = exprs ++ getApplicable(e2)
      }
      case Not(e1) => {
        exprs = exprs ++ getApplicable(e1)
      }
      case _ => {
        // Do nothing
      }
    }

    return exprs
  }
}
