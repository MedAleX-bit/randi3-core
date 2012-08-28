package org.randi3.model

import scala.collection.mutable.ListBuffer
import org.randi3.model.Entity._
import scalaz._
import Scalaz._
import org.joda.time.DateTime

case class TrialSubject private(id: Int, version: Int, createdAt: DateTime, identifier: String, investigatorUserName: String, trialSite: TrialSite, properties: List[SubjectProperty[_ <: Any]], stages: Map[String, List[SubjectProperty[_ <: Any]]], private val dummy: Any) extends Entity {


  def getStratum: String = {
    val strata = new ListBuffer[String]()
    properties.foreach {
      property =>
        val stratum = property.getStratum
        if (stratum.isDefined)
          strata.append(property.criterion.id + "_" + property.getStratum.get.id)
    }
    if(strata.size > 1)
     strata.toList.sortWith((e1, e2) => (e1 compareTo e2) < 0).reduce((acc, element) => acc + ";" + element)
    else  if(strata.size == 1)
     strata.head
    else ""
  }

}

object TrialSubject {

  def apply(id: Int = Int.MinValue, version: Int = 0, createdAt: DateTime = new DateTime(), identifier: String, investigatorUserName: String, trialSite: TrialSite, properties: List[SubjectProperty[_ <: Any]], stages: Map[String, List[SubjectProperty[_ <: Any]]] = Map()): ValidationNEL[String, TrialSubject] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkNotNull(createdAt),
      checkStringBetween(identifier, 1, maxTextLength),
      checkStringBetween(investigatorUserName, 1, maxTextLength),
      checkNotNull(trialSite),
      checkNotNull(properties),
      checkNotNull(stages)).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new TrialSubject(id, version, createdAt, identifier, investigatorUserName, trialSite, properties, stages, null))
    }
  }

  private val validSubject = new TrialSubject(Int.MinValue, 0, new DateTime(), "identifier", "userName", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassword").toOption.get, Nil, Map(), null)

  def check(id: Int = validSubject.id, version: Int = validSubject.version, createdAt: DateTime = validSubject.createdAt, identifier: String = validSubject.identifier, investigatorUserName: String = validSubject.investigatorUserName, trialSite: TrialSite = validSubject.trialSite, properties: List[SubjectProperty[_ <: Any]] = validSubject.properties, stages: Map[String, List[SubjectProperty[_ <: Any]]] = validSubject.stages): ValidationNEL[String, Boolean] = {
    apply(id, version, createdAt, identifier, investigatorUserName, trialSite, properties, stages).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }
}
