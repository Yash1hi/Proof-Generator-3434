package Math

class State(expr: Expr, operation: Operation) {
  val children: Seq[State] = Vector.empty[State]

  override def equals(obj: Any): Boolean = obj match {
    case that: State =>
      this.getExpr() == that.getExpr()
    case _ => false
  }

  def getExpr(): Expr = {
    return this.expr
  }
}
