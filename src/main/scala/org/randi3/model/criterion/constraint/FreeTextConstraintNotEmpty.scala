package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._

case class FreeTextConstraintNotEmpty private(id: Int, version: Int, configurations: List[Option[String]], private val dummy: Any) extends FreeTextConstraint {

   def isValueCorrect(value: String): Boolean = {
      if (value != null && !value.isEmpty) true
     else false
  }
}

object FreeTextConstraintNotEmpty {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[String]] = List()): ValidationNel[String, FreeTextConstraintNotEmpty] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsExact(configurations, 0)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new FreeTextConstraintNotEmpty(id, version, configurations, null))
    }
  }

  private def validConstraint = new FreeTextConstraintNotEmpty(Int.MinValue, 0, List(), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[String]] = validConstraint.configurations): ValidationNel[String, Boolean] = {
    apply(id, version, configurations).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}