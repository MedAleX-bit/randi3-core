package org.randi3.model

import Entity._
import scalaz._
import Scalaz._
import java.util.Date
import org.joda.time.LocalDate

case class TrialRight private(role: Role.Value, trial: Trial, private val dummy: Any) {

}

object TrialRight {

  def apply(role: Role.Value, trial: Trial): ValidationNEL[String, TrialRight] = {
    checkAll(
      checkNotNull(role),
      checkNotNull(trial)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new TrialRight(role, trial, null))
    }
  }

  private val validRight = new TrialRight(Role.investigator, Trial(Int.MinValue, 0, "validName", "validAbb", "validDescription", new LocalDate(1), new LocalDate(2), TrialStatus.ACTIVE, Nil, Nil, Nil, None, Map(), TrialSubjectIdentificationCreationType.CONTINUOUS_COUNTER, false, false, false).toOption.get, null)

  def check(role: Role.Value = validRight.role, trial: Trial = validRight.trial): ValidationNEL[String, Boolean] = {
    apply(role, trial).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }
}