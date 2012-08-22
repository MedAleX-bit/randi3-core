package org.randi3.model

object TrialSubjectIdentificationCreator {


  def createIdentification(trial: Trial, arm: TreatmentArm, subject:  TrialSubject): String = {
    trial.identificationCreationType match {
      case TrialSubjectIdentificationCreationType.CONTINUOUS_COUNTER => getContinuousCounter(trial, arm, subject)
      case TrialSubjectIdentificationCreationType.EXTERNAL => subject.identifier
      case TrialSubjectIdentificationCreationType.TRIAL_ARM_COUNTER =>  getTrialArmCounter(trial, arm, subject)
    }
  }

  private def getContinuousCounter(trial: Trial, arm: TreatmentArm, subject:  TrialSubject): String = {
    (trial.getSubjects.size).toString
  }

  private def getTrialArmCounter(trial: Trial, arm: TreatmentArm, subject:  TrialSubject): String = {
    trial.abbreviation+"_"+arm.name+"_"+arm.subjects.size
  }

}
