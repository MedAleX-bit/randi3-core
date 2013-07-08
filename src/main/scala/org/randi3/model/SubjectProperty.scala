package org.randi3.model

import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import Entity._
import scalaz._
import Scalaz._

case class SubjectProperty[T] private(id: Int, version: Int, criterion: Criterion[T, Constraint[T]], value: T, private val dummy: Any) extends Entity {


  def getStratum: Option[Constraint[T]] = {
    criterion.stratify(value)
  }
}

object SubjectProperty {

  def apply[T](id: Int = Int.MinValue, version: Int = 0, criterion: Criterion[T, Constraint[T]], value: T): ValidationNel[String, SubjectProperty[T]] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkNotNull(criterion),
      checkNotNull(value),
      checkValueCorrect(criterion, value)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new SubjectProperty(id, version, criterion, value, null))
    }
  }

  def check[T](id: Int = Int.MinValue, version: Int = 0, criterion: Criterion[T, Constraint[T]], value: T): ValidationNel[String, Boolean] = {
    apply(id, version, criterion, value).toEither match {
      case Left(x) => Failure(x)
      case Right(b) => Success(true)
    }
  }

  private def checkValueCorrect[T](criterion: Criterion[T, Constraint[T]], value: T): ValidationNel[String, Boolean] = {
    if (criterion.isValueCorrect(value)) Success(true)
    else Failure("Value is not correct").toValidationNel
  }

}
