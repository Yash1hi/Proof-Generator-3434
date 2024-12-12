package Math

class State(expr: Expr, operation: Operation) {
  var children: Vector[State] = Vector.empty[State]

  override def equals(obj: Any): Boolean = obj match {
    case that: State =>
      this.getExpr() == that.getExpr()
    case _ => false
  }

  def getExpr(): Expr = {
    this.expr
  }

  def getChildren(): Vector[State] = {
    children
  }
  
  def getOperation(): Operation = {
    operation
  }

  def expandState(rules: Vector[Theorem]): Vector[State] = {
   for (rule <- rules) {
     for ((appliedTo, newExpr) <- rule.getApplicable(this.expr)) {
       val op = Operation(this, rule, appliedTo)
       val childState = State(newExpr, op)
       children = children :+ childState
     }
   }
    
   return getChildren()
  }

}
