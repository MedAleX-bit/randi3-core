package org.randi3.model.criterion

import org.randi3.model.criterion.constraint.DateConstraint
import org.randi3.model.Entity._
import scalaz._
import Scalaz._
import org.joda.time.LocalDate

case class DateCriterion private(id: Int, version: Int, name: String, description: String, inclusionConstraint: Option[DateConstraint], strata: List[DateConstraint], private val dummy: Any) extends Criterion[LocalDate, DateConstraint] {

}

object DateCriterion {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, inclusionConstraint: Option[DateConstraint], strata: List[DateConstraint]): ValidationNEL[String, DateCriterion] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(description, 2, maxTextLength),
      checkNotNull(inclusionConstraint),
      checkNotNull(strata)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new DateCriterion(id, version, name, description, inclusionConstraint, strata, null))
    }
  }

  private def validCriterion = new DateCriterion(Int.MinValue, 0, "validName", "validDescription", None, Nil, null)

  def check(id: Int = validCriterion.id, version: Int = validCriterion.version, name: String = validCriterion.name, description: String = validCriterion.description, inclusionConstraint: Option[DateConstraint] = validCriterion.inclusionConstraint, strata: List[DateConstraint] = validCriterion.strata): ValidationNEL[String, Boolean] = {
    apply(id, version, name, description, inclusionConstraint, strata).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}

