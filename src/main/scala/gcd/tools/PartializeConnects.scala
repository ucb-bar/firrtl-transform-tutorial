// See README.md for license details.

package gcd.tools

import firrtl._
import firrtl.ir._
import firrtl.Mappers._

class PartializeConnects extends Transform {
  val inputForm = LowForm
  val outputForm = HighForm

  def transformStmt(stmt: Statement): Statement = stmt match {
    case Connect(info, loc, expr) =>
      PartialConnect(info, loc, expr)
    case s => s.map(transformStmt)
  }

  def transformMod(m: DefModule): DefModule = m.map(transformStmt)

  def execute(state: CircuitState): CircuitState = {
    val transformed = state.circuit.map(transformMod)
    state.copy(circuit = transformed)
  }
}
