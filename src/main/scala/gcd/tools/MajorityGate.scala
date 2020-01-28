// See README.md for license details.

package gcd.tools

import firrtl._
import firrtl.ir._

object MajorityGate {
  private def and(a: Expression, b: Expression) = DoPrim(PrimOps.And, Seq(a, b), Nil, a.tpe)
  private def or(a: Expression, b: Expression) = DoPrim(PrimOps.Or, Seq(a, b), Nil, a.tpe)

  def apply(a: Expression, b: Expression, c: Expression): Expression = {
    // To make this reusable, we can't rely on it being called from a low-form pass
    require(a.tpe == b.tpe && b.tpe == c.tpe)
    require(a.tpe.isInstanceOf[GroundType])
    or(or(and(a, b), and(b, c)), and(a, c))
  }
}
