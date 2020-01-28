// See README.md for license details.

package gcd.tools

import firrtl._

class ResilientRegisterFlow extends SeqTransform {
  def inputForm = LowForm
  def outputForm = LowForm

  def transforms = Seq(ResilientRegisters, new ResolveAndCheck(), passes.SplitExpressions, InsertSoftErrorModels, new ResolveAndCheck())
}
