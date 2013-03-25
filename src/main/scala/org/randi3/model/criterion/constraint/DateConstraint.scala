package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._
import org.joda.time.{Interval, LocalDate}


/**
 * The list have to contained two elements
 * - if the first element is null the constraint has no lower bound
 * - if the second elemnt is null the constraint has no upper bound
 * - if both values are set the constraint defined a range
 */
case class DateConstraint private(id: Int, version: Int, configurations: List[Option[LocalDate]], private val dummy: Any) extends Constraint[LocalDate] {

  val firstValue = configurations(0)

  val secondValue = configurations(1)


  def isValueCorrect(value: LocalDate): Boolean = {
    if (firstValue.isEmpty) value.isBefore(secondValue.get)
    else
    if (secondValue.isEmpty) value.isAfter(firstValue.get)
    else
      value.isAfter(firstValue.get) && value.isBefore(secondValue.get)
  }
}


object DateConstraint {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[LocalDate]]): ValidationNEL[String, DateConstraint] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsExact(configurations, 2),
      checkAtLeastOneElementIsDefined(configurations)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new DateConstraint(id, version, configurations, null))
    }
  }

  private def validConstraint = new DateConstraint(Int.MinValue, 0, List(None, Some(new LocalDate)), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[LocalDate]] = validConstraint.configurations): ValidationNEL[String, Boolean] = {
    apply(id, version, configurations).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
