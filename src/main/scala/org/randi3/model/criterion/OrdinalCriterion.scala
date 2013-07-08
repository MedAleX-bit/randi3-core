package org.randi3.model.criterion

import org.randi3.model.criterion.constraint.OrdinalConstraint
import org.randi3.model.Entity._
import scalaz._
import Scalaz._

case class OrdinalCriterion private(id: Int, version: Int, name: String, description: String, values: Set[String], inclusionConstraint: Option[OrdinalConstraint], strata: List[OrdinalConstraint], private val dummy: Any) extends Criterion[String, OrdinalConstraint] {

}

object OrdinalCriterion {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, values: Set[String], inclusionConstraint: Option[OrdinalConstraint], strata: List[OrdinalConstraint]): ValidationNel[String, OrdinalCriterion] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(description, 2, maxTextLength),
      checkListContainsMin(values.toList, 2),
      checkNotNull(inclusionConstraint),
      checkNotNull(strata)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new OrdinalCriterion(id, version, name, description, values, inclusionConstraint, strata, null))
    }
  }

  private def validCriterion = new OrdinalCriterion(Int.MinValue, 0, "validName", "validDescription", Set("a", "b"), None, Nil, null)

  def check(id: Int = validCriterion.id, version: Int = validCriterion.version, name: String = validCriterion.name, description: String = validCriterion.description, values: Set[String] = validCriterion.values, inclusionConstraint: Option[OrdinalConstraint] = validCriterion.inclusionConstraint, strata: List[OrdinalConstraint] = validCriterion.strata): ValidationNel[String, Boolean] = {
    apply(id, version, name, description, values, inclusionConstraint, strata).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
