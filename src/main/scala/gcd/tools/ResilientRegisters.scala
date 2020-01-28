// See README.md for license details.

package gcd.tools

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations._

case class ResilientRegisterAnnotation(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(newTarget: ReferenceTarget): ResilientRegisterAnnotation = {
    this.copy(target = newTarget)
  }
}

object ResilientRegisters extends Transform {
  val inputForm = LowForm
  val outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    state
  }
}
