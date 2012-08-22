package org.randi3.model.criterion

import constraint.IntegerConstraint
import scalaz._
import Scalaz._
import org.randi3.model.Entity._

case class IntegerCriterion private(id: Int, version: Int, name: String, description: String, inclusionConstraint: Option[IntegerConstraint], strata: List[IntegerConstraint], private val dummy: Any) extends Criterion[Int, IntegerConstraint] {

}

object IntegerCriterion {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, inclusionConstraint: Option[IntegerConstraint], strata: List[IntegerConstraint]): ValidationNEL[String, IntegerCriterion] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(description, 2, maxTextLength),
      checkNotNull(inclusionConstraint),
      checkNotNull(strata)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new IntegerCriterion(id, version, name, description, inclusionConstraint, strata, null))
    }
  }

  private def validCriterion = new IntegerCriterion(Int.MinValue, 0, "validName", "validDescription", None, Nil, null)

  def check(id: Int = validCriterion.id, version: Int = validCriterion.version, name: String = validCriterion.name, description: String = validCriterion.description, inclusionConstraint: Option[IntegerConstraint] = validCriterion.inclusionConstraint, strata: List[IntegerConstraint] = validCriterion.strata): ValidationNEL[String, Boolean] = {
    apply(id, version, name, description, inclusionConstraint, strata).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
