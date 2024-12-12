package Math

import scala.collection.mutable.Queue

class Prover(theorems: Vector[Theorem], startExpr: Expr, endExpr: Expr) {
  var existingGraph: Vector[State] = Vector.empty[State]
  val toExpand: Queue[State] = Queue[State](State(startExpr, null))

  def bfs(): State = {
    while (toExpand.nonEmpty) {
      val currentState = toExpand.dequeue()

      if (currentState.getExpr() == endExpr) {
        return currentState
      }

      val newStates = currentState.expandState(theorems)

      for (newState <- newStates if !existingGraph.contains(newState)) {
        toExpand.enqueue(newState)
        existingGraph = existingGraph :+ newState
      }
    }

    // failed to generate proof.
    null
  }
}
