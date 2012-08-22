package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._

case class FreeTextConstraint private(id: Int, version: Int, configurations: List[Option[String]], private val dummy: Any) extends Constraint[String] {

  val expectedValue = configurations(0).get

  def isValueCorrect(value: String): Boolean = {
    value.equals(expectedValue)
  }
}

object FreeTextConstraint {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[String]]): ValidationNEL[String, FreeTextConstraint] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsExact(configurations, 1),
      checkAllElementsDefined(configurations)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new FreeTextConstraint(id, version, configurations, null))
    }
  }

  private def validConstraint = new FreeTextConstraint(Int.MinValue, 0, List(Some("a")), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[String]] = validConstraint.configurations): ValidationNEL[String, Boolean] = {
    apply(id, version, configurations).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}