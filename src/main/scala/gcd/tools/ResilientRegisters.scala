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
    stmt match {
      case reg: DefRegister if (copyNameMap.contains(reg.name)) =>
        // Make three copies of the register with new names
        val (name0, name1, name2) = copyNameMap(reg.name)
        val c0 = renameReg(reg, name0)
        val c1 = renameReg(reg, name1)
        val c2 = renameReg(reg, name2)
        // Make a majority-valued node that shares the register's original name
        val majExpr = MajorityGate(WRef(c0), WRef(c1), WRef(c2))
        val majNode = DefNode(reg.info, reg.name, majExpr)
        Block(Seq(c0, c1, c2, majNode))
      case Connect(info, WRef(name, tpe, RegKind, SinkFlow), rhs) if (copyNameMap.contains(name)) =>
        val (name0, name1, name2) = copyNameMap(name)
        val conn0 = Connect(info, WRef(name0, tpe, RegKind, SinkFlow), rhs)
        val conn1 = Connect(info, WRef(name1, tpe, RegKind, SinkFlow), rhs)
        val conn2 = Connect(info, WRef(name2, tpe, RegKind, SinkFlow), rhs)
        Block(Seq(conn0, conn1, conn2))
      case s => s.map(makeRegTriples(copyNameMap))
    }
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
