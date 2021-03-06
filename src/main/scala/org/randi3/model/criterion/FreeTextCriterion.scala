package org.randi3.model.criterion

import org.randi3.model.Entity._
import constraint.FreeTextConstraint
import constraint.FreeTextConstraintExact
import scalaz._
import Scalaz._

case class FreeTextCriterion private(id: Int, version: Int, name: String, description: String, inclusionConstraint: Option[FreeTextConstraint], strata: List[FreeTextConstraintExact], private val dummy: Any) extends Criterion[String, FreeTextConstraint] {

  override def isValueCorrect(value: String): Boolean = {
    if (value == null) return false
    super.isValueCorrect(value)
  }
}


object FreeTextCriterion {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, inclusionConstraint: Option[FreeTextConstraint], strata: List[FreeTextConstraintExact]): ValidationNel[String, FreeTextCriterion] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(description, 2, maxTextLength),
      checkNotNull(inclusionConstraint),
      checkNotNull(strata)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new FreeTextCriterion(id, version, name, description, inclusionConstraint, strata, null))
    }
  }

  private def validCriterion = new FreeTextCriterion(Int.MinValue, 0, "validName", "validDescription", None, Nil, null)

  def check(id: Int = validCriterion.id, version: Int = validCriterion.version, name: String = validCriterion.name, description: String = validCriterion.description, inclusionConstraint: Option[FreeTextConstraint] = validCriterion.inclusionConstraint, strata: List[FreeTextConstraintExact] = validCriterion.strata): ValidationNel[String, Boolean] = {
    apply(id, version, name, description, inclusionConstraint, strata).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
