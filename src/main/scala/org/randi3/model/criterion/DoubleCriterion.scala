package org.randi3.model.criterion

import org.randi3.model.criterion.constraint.DoubleConstraint
import org.randi3.model.Entity._
import scalaz._
import Scalaz._

case class DoubleCriterion private(id: Int, version: Int, name: String, description: String, inclusionConstraint: Option[DoubleConstraint], strata: List[DoubleConstraint], private val dummy: Any) extends Criterion[Double, DoubleConstraint] {

}

object DoubleCriterion {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, inclusionConstraint: Option[DoubleConstraint], strata: List[DoubleConstraint]): ValidationNEL[String, DoubleCriterion] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(description, 2, maxTextLength),
      checkNotNull(inclusionConstraint),
      checkNotNull(strata)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new DoubleCriterion(id, version, name, description, inclusionConstraint, strata, null))
    }
  }

  private def validCriterion = new DoubleCriterion(Int.MinValue, 0, "validName", "validDescription", None, Nil, null)

  def check(id: Int = validCriterion.id, version: Int = validCriterion.version, name: String = validCriterion.name, description: String = validCriterion.description, inclusionConstraint: Option[DoubleConstraint] = validCriterion.inclusionConstraint, strata: List[DoubleConstraint] = validCriterion.strata): ValidationNEL[String, Boolean] = {
    apply(id, version, name, description, inclusionConstraint, strata).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
