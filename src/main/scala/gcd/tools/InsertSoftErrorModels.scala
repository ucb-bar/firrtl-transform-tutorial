// See README.md for license details.

package gcd.tools

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations._

import collection.mutable

case class SoftErrorRegisterAnnotation(target: ReferenceTarget, errorPPM: BigInt) extends SingleTargetAnnotation[ReferenceTarget] {
  require(target.component.isEmpty)
  def duplicate(newTarget: ReferenceTarget): SoftErrorRegisterAnnotation = {
    this.copy(target = newTarget)
  }
}

object SoftErrorModel {
  val defname = "SoftErrorFlopModel"
  def clock = Port(NoInfo, "clock", Input, ClockType)
  def d = Port(NoInfo, "D", Input, UIntType(UnknownWidth))
  def q = Port(NoInfo, "Q", Output, UIntType(UnknownWidth))
  def decl(dType: Type, errorPPM: BigInt, name: String): ExtModule = {
    dType match {
      case UIntType(IntWidth(w)) =>
        val ports = Seq(clock, d.copy(tpe = dType), q.copy(tpe = dType))
        ExtModule(NoInfo, name, ports, defname, Seq(IntParam("WIDTH", w), IntParam("ERROR_PPM", errorPPM)))
      case _ =>
        throw new Exception("Soft error models current support only UIntType of known width")
    }
  }
}

object InsertSoftErrorModels extends Transform {
  val inputForm = LowForm
  val outputForm = LowForm

  private def replaceRegRefs(mt: ModuleTarget, bbInfo: Map[ReferenceTarget, (String, BigInt)])(expr: Expression): Expression = {
    expr match {
      case WRef(name, tpe, RegKind, SourceFlow) if (bbInfo.contains(mt.ref(name))) =>
        WSubField(WRef(name), "Q")
      case e => e.map(replaceRegRefs(mt, bbInfo))
    }
  }

  private def replaceRegisters(mt: ModuleTarget, bbInfo: Map[ReferenceTarget, (String, BigInt)], bbDecls: mutable.Set[ExtModule])(stmt: Statement): Statement = {
    stmt.map(replaceRegRefs(mt, bbInfo)) match {
      case DefRegister(info, name, tpe, clock, reset, init) if (bbInfo.contains(mt.ref(name))) =>
        require(reset == UIntLiteral(0), "Soft error models currently do not support reset values")
        val (moduleName, errorPPM) = bbInfo(mt.ref(name))
        val decl = SoftErrorModel.decl(tpe, errorPPM, moduleName)
        val inst = WDefInstance(info, name, moduleName, UnknownType)
        val conn = Connect(info, WSubField(WRef(inst), SoftErrorModel.clock.name), clock)
        bbDecls += decl
        Block(Seq(inst, conn))
      case Connect(info, WRef(name, tpe, RegKind, SinkFlow), rhs) if (bbInfo.contains(mt.ref(name))) =>
        Connect(info, WSubField(WRef(name), "D"), rhs)
      case s => s.map(replaceRegisters(mt, bbInfo, bbDecls))
    }
  }

  def execute(state: CircuitState): CircuitState = {
    val moduleNS = Namespace(state.circuit)
    val softErrorRegInfo = state.annotations.collect {
      case SoftErrorRegisterAnnotation(rt, errorPPM) =>
        require(rt.isLocal)
        rt -> (moduleNS.newName(SoftErrorModel.defname), errorPPM)
    }

    val bbInfo = softErrorRegInfo.toMap
    val bbDecls = new mutable.LinkedHashSet[ExtModule]

    val transformedModules = state.circuit.modules.map {
      case m =>
        val mt = ModuleTarget(state.circuit.main, m.name)
        m.map(replaceRegisters(mt, bbInfo, bbDecls))
    }

    val transformedCircuit = state.circuit.copy(modules = transformedModules ++ bbDecls)
    val filteredAnnos = state.annotations.filterNot(_.isInstanceOf[SoftErrorRegisterAnnotation])
    state.copy(transformedCircuit, outputForm, filteredAnnos, None)
  }
}
