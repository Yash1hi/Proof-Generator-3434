package Math

class Operation(prevState: State, appliedTheorem: Theorem, appliedSection: Expr) {

  def getPreviousState: State = {
    prevState
  }
  
  def getAppliedTheorem: Theorem = {
    appliedTheorem
  }
  
  def getAppliedSection: Expr = {
    appliedSection
  }
}
