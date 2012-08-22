package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._


case class OrdinalConstraint private(id: Int, version: Int, configurations: List[Option[String]], private val dummy: Any) extends Constraint[String] {

  val expectedValues = configurations.filter(element => element.isDefined).map(element => element.get).toSet

  def isValueCorrect(value: String): Boolean = {
    if (expectedValues.contains(value)) true else false
  }
}

object OrdinalConstraint {

  def apply(id: Int = Int.MinValue, version: Int = 0, configurations: List[Option[String]]): ValidationNEL[String, OrdinalConstraint] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkListContainsMin(configurations, 1),
      checkAllElementsDefined(configurations)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new OrdinalConstraint(id, version, configurations, null))
    }
  }

  private def validConstraint = new OrdinalConstraint(Int.MinValue, 0, List(Some("a"), Some("b")), null)

  def check(id: Int = validConstraint.id, version: Int = validConstraint.version, configurations: List[Option[String]] = validConstraint.configurations): ValidationNEL[String, Boolean] = {
    apply(id, version, configurations).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

}
