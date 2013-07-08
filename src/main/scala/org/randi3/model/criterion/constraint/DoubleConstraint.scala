package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._


/**
 * The list have to contained two elements
 * - if the first element is null the constraint has no lower bound
 * - if the second elemnt is null the constraint has no upper bound
 * - if both values are set the constraint defined a range
 */
case class DoubleConstraint private(id: Int, version: Int, configurations: List[Option[Double]], private val dummy: Any) extends Constraint[Double] {

  val firstValue = configurations(0)

  val secondValue = configurations(1)

  def isValueCorrect(value: Double): Boolean = {
    if (firstValue.isEmpty) value <= secondValue.get
    else
    if (secondValue.isEmpty) value >= firstValue.get
    else
      value >= firstValue.get && value <= secondValue.get
  }
}

object DoubleConstraint {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[Double]]): ValidationNel[String, DoubleConstraint] = {
    //TODO check that only one entry can be empty
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsExact(configurations, 2),
      checkAtLeastOneElementIsDefined(configurations)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new DoubleConstraint(id, version, configurations, null))
    }
  }

  private def validConstraint = new DoubleConstraint(Int.MinValue, 0, List(None, Some(0.1)), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[Double]] = validConstraint.configurations): ValidationNel[String, Boolean] = {
    apply(id, version, configurations).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
