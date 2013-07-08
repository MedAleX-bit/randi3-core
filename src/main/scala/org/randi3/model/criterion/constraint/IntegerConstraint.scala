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
case class IntegerConstraint private(id: Int, version: Int, configurations: List[Option[Int]], private val dummy: Any) extends Constraint[Int] {

  val firstValue = configurations(0)

  val secondValue = configurations(1)

  def isValueCorrect(value: Int): Boolean = {
    if (firstValue.isEmpty) value <= secondValue.get
    else
    if (secondValue.isEmpty) value >= firstValue.get
    else
      value >= firstValue.get && value <= secondValue.get
  }
}


object IntegerConstraint {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[Int]]): ValidationNel[String, IntegerConstraint] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsExact(configurations, 2),
      checkAtLeastOneElementIsDefined(configurations)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new IntegerConstraint(id, version, configurations, null))
    }
  }

  private def validConstraint = new IntegerConstraint(Int.MinValue, 0, List(None, Some(10)), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[Int]] = validConstraint.configurations): ValidationNel[String, Boolean] = {
    apply(id, version, configurations).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
