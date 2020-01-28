// See README.md for license details.

package gcd.tools

import firrtl._
import firrtl.ir._
import firrtl.Mappers._

class RemoveTrivialPartialConnects extends Transform {
  val inputForm = HighForm
  val outputForm = HighForm

  def transformStmt(stmt: Statement): Statement = stmt match {
    case PartialConnect(info, l, r) =>
      ???
    case s => s.map(transformStmt)
  }

  def transformMod(m: DefModule): DefModule = m.map(transformStmt)

  def execute(state: CircuitState): CircuitState = {
    val transformed = state.circuit.map(transformMod)
    state.copy(circuit = transformed)
  }
}
