package org.randi3.model

import scala.collection.mutable.ListBuffer
import Entity._
import scalaz._
import Scalaz._

case class TreatmentArm private(id: Int, version: Int, name: String, description: String, subjects: ListBuffer[TrialSubject], plannedSize: Int, private val dummy: Any) extends Entity {

  def addSubject(subject: TrialSubject) = {
    subjects += subject
  }
}

object TreatmentArm {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, subjects: ListBuffer[TrialSubject] = new ListBuffer(), plannedSize: Int): ValidationNEL[String, TreatmentArm] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(description, 1, maxTextLength),
      checkNotNull(subjects),
      checkIntMin(plannedSize, 1)).either match {
      case Right(_) => Success(new TreatmentArm(id, version, name, description, subjects, plannedSize, null))
      case Left(x) => Failure(x)
    }
  }

  private val validArm = new TreatmentArm(Int.MinValue, 0, "validName", "vaildDescription", new ListBuffer[TrialSubject], 100, null)

  def check(id: Int = validArm.id, version: Int = validArm.version, name: String = validArm.name, description: String = validArm.description, subjects: ListBuffer[TrialSubject] = validArm.subjects, plannedSize: Int = validArm.plannedSize): ValidationNEL[String, Boolean] = {
    apply(id, version, name, description, subjects, plannedSize).either match {
      case Right(_) => Success(true)
      case Left(x) => Failure(x)
    }
  }
}
