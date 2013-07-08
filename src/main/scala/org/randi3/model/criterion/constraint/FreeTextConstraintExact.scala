package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._

case class FreeTextConstraintExact private(id: Int, version: Int, configurations: List[Option[String]], private val dummy: Any) extends FreeTextConstraint {

  val expectedValue = configurations(0).get

  def isValueCorrect(value: String): Boolean = {
    value.equals(expectedValue)
  }
}

object FreeTextConstraintExact {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[String]]): ValidationNel[String, FreeTextConstraintExact] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsExact(configurations, 1),
      checkAllElementsDefined(configurations)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new FreeTextConstraintExact(id, version, configurations, null))
    }
  }

  private def validConstraint = new FreeTextConstraintExact(Int.MinValue, 0, List(Some("a")), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[String]] = validConstraint.configurations): ValidationNel[String, Boolean] = {
    apply(id, version, configurations).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}