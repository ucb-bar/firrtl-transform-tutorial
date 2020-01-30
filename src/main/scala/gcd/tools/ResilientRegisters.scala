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

  type TripleCopyNameMap = Map[String, Tuple3[String, String, String]]

  private def renameReg(reg: DefRegister, newName: String): DefRegister = {
    val renamed = reg.copy(name = newName)
    if (renamed.reset == UIntLiteral(0)) {
      renamed.copy(init = WRef(renamed))
    }  else {
      renamed
    }
  }

  private def makeRegTriples(copyNameMap: TripleCopyNameMap)(stmt: Statement): Statement = {
    stmt
  }

  private def regTripleNames(ns: Namespace, regName: String): (String, String, String) = {
    (ns.newName(s"${regName}_copy_0"), ns.newName(s"${regName}_copy_1"), ns.newName(s"${regName}_copy_2"))
  }

  def execute(state: CircuitState): CircuitState = {
    val resilientRegTargets = state.annotations.collect {
      case ResilientRegisterAnnotation(target) =>
        require(target.isLocal)
        target
    }

    val resilientRegsByModule = resilientRegTargets.groupBy(_.module)

    // We need to record that we've split each register into three new ones
    val renames = RenameMap()

    val transformedModules = state.circuit.modules.map {
      case m: DefModule if (resilientRegsByModule.contains(m.name)) =>
        val ns = Namespace(m)
        val resilientRegs = resilientRegsByModule(m.name)
        val copyMap = resilientRegs.map(rTarget => rTarget -> regTripleNames(ns, rTarget.ref)).toMap
        val copyNameMap = copyMap.map { case (k, v) => k.ref -> v }
        copyMap.foreach {
          case (oldTarget, (copyName0, copyName1, copyName2)) =>
            val newTargets = Seq(copyName0, copyName1, copyName2).map(name => oldTarget.copy(ref = name))
            renames.record(oldTarget, newTargets)
        }
        m.map(makeRegTriples(copyNameMap))
      case m => m
    }

    val transformedCircuit = state.circuit.copy(modules = transformedModules)
    val filteredAnnos = state.annotations.filterNot(_.isInstanceOf[ResilientRegisterAnnotation])
    state.copy(transformedCircuit, outputForm, filteredAnnos, Some(renames))
  }
}
