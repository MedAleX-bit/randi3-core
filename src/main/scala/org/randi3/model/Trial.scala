package org.randi3.model

import org.joda.time.LocalDate

import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.randomization.RandomizationMethod
import scalaz._
import Scalaz._
import Entity._

case class Trial private(id: Int, version: Int, name: String, abbreviation: String, description: String, startDate: LocalDate, endDate: LocalDate, stratifyTrialSite: StratifiedTrialSite.Value, status: TrialStatus.Value, treatmentArms: List[TreatmentArm], criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]], participatingSites: List[TrialSite], randomizationMethod: Option[RandomizationMethod], stages: Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]], identificationCreationType: TrialSubjectIdentificationCreationType.Value, private val dummy: Any) extends Entity {

  def randomize(subject: TrialSubject): Validation[String, TreatmentArm] = {
    if (randomizationMethod.isDefined) {
      val arm = randomizationMethod.get.randomize(this, subject)
      if(arm != null)
      arm.addSubject(subject)
      Success(arm)
    } else Failure("No randomisation method is defined")
  }

  def plannedSubjects: Int = {
    treatmentArms.map(arm => arm.plannedSize).reduce((acc, entry) => acc + entry)
  }

  def getSubjects: List[TrialSubject] = {
    treatmentArms.map(arm => arm.subjects.toList).reduce((acc, subjects) => acc ::: subjects)
  }

  def isStratified: Boolean = {
    criterions.map(criterion => !criterion.strata.isEmpty).reduce((acc, element) => acc || element)
  }

}

object Trial {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, abbreviation: String, description: String, startDate: LocalDate, endDate: LocalDate, stratifyTrialSite: StratifiedTrialSite.Value, status: TrialStatus.Value, treatmentArms: List[TreatmentArm], criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]], participatingSites: List[TrialSite], randomizationMethod: Option[RandomizationMethod], stages: Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]], identificationCreationType: TrialSubjectIdentificationCreationType.Value): ValidationNEL[String, Trial] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(abbreviation, 1, 50),
      checkStringBetween(description, 1, maxTextLength),
      checkStartEnd(startDate, endDate),
      checkNotNull(stratifyTrialSite),
      checkNotNull(status),
      checkNotNull(participatingSites),
      checkNotNull(randomizationMethod),
      checkNotNull(stages),
      checkNotNull(treatmentArms),
      checkNotNull(criterions),
      checkNotNull(identificationCreationType)).either match {
      case Right(_) => Success(new Trial(id, version, name, abbreviation, description, startDate, endDate, stratifyTrialSite, status, treatmentArms, criterions, participatingSites, randomizationMethod, stages, identificationCreationType, null))
      case Left(x) => Failure(x)
    }
  }

  private val validTrial = new Trial(Int.MinValue, 0, "validName", "validAbb", "validDescription", new LocalDate(1900, 10, 1), new LocalDate(5000, 2, 1), StratifiedTrialSite.NO, TrialStatus.IN_PREPARATION, Nil, Nil, Nil, None, Map(), TrialSubjectIdentificationCreationType.CONTINUOUS_COUNTER, null)

  def check(id: Int = validTrial.id, version: Int = validTrial.version, name: String = validTrial.name, abbreviation: String = validTrial.abbreviation, description: String = validTrial.description, startDate: LocalDate = validTrial.startDate, endDate: LocalDate = validTrial.endDate, stratifyTrialSite: StratifiedTrialSite.Value = validTrial.stratifyTrialSite, status: TrialStatus.Value = validTrial.status, treatmentArms: List[TreatmentArm] = validTrial.treatmentArms, criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]] = validTrial.criterions, participatingSites: List[TrialSite] = validTrial.participatingSites, randomizationMethod: Option[RandomizationMethod] = validTrial.randomizationMethod, stages: Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]] = validTrial.stages, identificationCreationType: TrialSubjectIdentificationCreationType.Value = validTrial.identificationCreationType): ValidationNEL[String, Boolean] = {
    apply(id, version, name, abbreviation, description, startDate, endDate, stratifyTrialSite, status, treatmentArms, criterions, participatingSites, randomizationMethod, stages, identificationCreationType).either match {
      case Right(_) => Success(true)
      case Left(x) => Failure(x)
    }
  }

}
