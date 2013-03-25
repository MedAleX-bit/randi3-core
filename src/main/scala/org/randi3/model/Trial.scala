package org.randi3.model

import org.joda.time.LocalDate

import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.randomization.RandomizationMethod
import scalaz._
import Scalaz._
import Entity._

case class Trial private(id: Int, version: Int, name: String, abbreviation: String, description: String, startDate: LocalDate, endDate: LocalDate, status: TrialStatus.Value, treatmentArms: List[TreatmentArm], criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]], participatingSites: List[TrialSite], randomizationMethod: Option[RandomizationMethod], stages: Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]], identificationCreationType: TrialSubjectIdentificationCreationType.Value, isEDCTrial: Boolean, isTrialOpen: Boolean, isStratifiedByTrialSite: Boolean, private val dummy: Any) extends Entity {

  def randomize(subject: TrialSubject): Validation[String, TreatmentArm] = {
    if (randomizationMethod.isDefined) {
      //check properties
      if (subject.properties.size != criterions.size) return Failure("Subject data not correct filled")

      if (subject.properties.size > 2) {
        if (!subject.properties.map(prop => criterions.map(_.id).contains(prop.criterion.id)).reduce((a, b) => a && b))
          return Failure("Subject data not correct filled")
      } else if (subject.properties.size == 1) {
        if (subject.properties.head.criterion.id != criterions.head.id)
          return Failure("Subject data not correct filled")
      }

      val arm = randomizationMethod.get.randomize(this, subject)
      if (arm != null) {
        arm.addSubject(subject)
        Success(arm)
      } else {
        Failure("Randomization method couldn't assign treatment arm, pleas contact an administrator")
      }
    } else Failure("No randomisation method is defined")
  }

  def plannedSubjects: Int = {
    treatmentArms.map(arm => arm.plannedSize).reduce((acc, entry) => acc + entry)
  }

  def getSubjects: List[TrialSubject] = {
    val subjects =  treatmentArms.map(arm => arm.subjects.toList)
    if (subjects.size > 1){
      subjects.reduce((acc, subjects) => acc ::: subjects)
    }else if (subjects.size == 1){
      subjects.head
    }else {
      List()
    }
  }

  def isStratified: Boolean = {
    criterions.map(criterion => !criterion.strata.isEmpty).reduce((acc, element) => acc || element)
  }

}

object Trial {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, abbreviation: String, description: String, startDate: LocalDate, endDate: LocalDate, status: TrialStatus.Value, treatmentArms: List[TreatmentArm], criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]], participatingSites: List[TrialSite], randomizationMethod: Option[RandomizationMethod], stages: Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]], identificationCreationType: TrialSubjectIdentificationCreationType.Value, isEDCTrial: Boolean = false, isTrialOpen: Boolean, isStratifiedByTrialSite: Boolean): ValidationNEL[String, Trial] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(abbreviation, 1, 50),
      checkStringBetween(description, 1, maxTextLength),
      checkStartEnd(startDate, endDate),
      checkNotNull(status),
      checkNotNull(participatingSites),
      checkNotNull(randomizationMethod),
      checkNotNull(stages),
      checkNotNull(treatmentArms),
      checkNotNull(criterions),
      checkNotNull(identificationCreationType)).either match {
      case Right(_) => Success(new Trial(id, version, name, abbreviation, description, startDate, endDate, status, treatmentArms, criterions, participatingSites, randomizationMethod, stages, identificationCreationType, isEDCTrial, isTrialOpen, isStratifiedByTrialSite, null))
      case Left(x) => Failure(x)
    }
  }

  private val validTrial = new Trial(Int.MinValue, 0, "validName", "validAbb", "validDescription", new LocalDate(1900, 10, 1), new LocalDate(5000, 2, 1), TrialStatus.IN_PREPARATION, Nil, Nil, Nil, None, Map(), TrialSubjectIdentificationCreationType.CONTINUOUS_COUNTER, false, false, false, null)

  def check(id: Int = validTrial.id, version: Int = validTrial.version, name: String = validTrial.name, abbreviation: String = validTrial.abbreviation, description: String = validTrial.description, startDate: LocalDate = validTrial.startDate, endDate: LocalDate = validTrial.endDate, status: TrialStatus.Value = validTrial.status, treatmentArms: List[TreatmentArm] = validTrial.treatmentArms, criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]] = validTrial.criterions, participatingSites: List[TrialSite] = validTrial.participatingSites, randomizationMethod: Option[RandomizationMethod] = validTrial.randomizationMethod, stages: Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]] = validTrial.stages, identificationCreationType: TrialSubjectIdentificationCreationType.Value = validTrial.identificationCreationType, isEDCTrial: Boolean = validTrial.isEDCTrial, isTrialOpen: Boolean = validTrial.isTrialOpen, isStratifiedByTrialSite: Boolean = validTrial.isStratifiedByTrialSite): ValidationNEL[String, Boolean] = {
    apply(id, version, name, abbreviation, description, startDate, endDate, status, treatmentArms, criterions, participatingSites, randomizationMethod, stages, identificationCreationType, isEDCTrial, isTrialOpen, isStratifiedByTrialSite).either match {
      case Right(_) => Success(true)
      case Left(x) => Failure(x)
    }
  }

}
