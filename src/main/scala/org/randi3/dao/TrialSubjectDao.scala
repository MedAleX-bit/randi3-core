package org.randi3.dao

import java.util.Date
import org.randi3.model.SubjectProperty
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.DateCriterion
import org.randi3.model.criterion.DoubleCriterion
import org.randi3.model.criterion.FreeTextCriterion
import org.randi3.model.criterion.IntegerCriterion
import org.randi3.model.criterion.OrdinalCriterion
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.model.TrialSubject
import org.scalaquery.ql.Parameters
import org.scalaquery.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.utility.UtilityDBComponent
import java.sql.Timestamp
import org.joda.time.DateTime

trait TrialSubjectDaoComponent {

  this: DaoComponent with
    CriterionDaoComponent with
    TrialSiteDaoComponent with
    UtilityDBComponent =>

  val trialSubjectDao: TrialSubjectDao

  class TrialSubjectDao {

    import driver.Implicit._
    import schema._
    import utilityDB._

    private val queryTrialSubjectFromId = for {
      id <- Parameters[Int]
      t <- TrialSubjects if t.id === id
    } yield t.id ~ t.version ~ t.createdAt ~ t.treatmentArmId ~ t.identifier ~ t.investigatorUserName ~ t.trialSiteId

    private def queryTrialSubjectFromIdentifierAndTrialId(identifier: String, trialId: Int) = for {
      arms <- TreatmentArms if arms.trialId === trialId
      t <- TrialSubjects if t.identifier === identifier && t.treatmentArmId === (arms.id)
    } yield t.id ~ t.version ~ t.createdAt ~ t.treatmentArmId ~ t.identifier ~ t.investigatorUserName ~ t.trialSiteId

    private def queryTrialSubjectFromTreatmentArmIdAndIdentifier(treatmentArmId: Int, identifier: String) = for {
      t <- TrialSubjects if t.treatmentArmId === treatmentArmId && t.identifier === identifier
    } yield t.id ~ t.version ~ t.createdAt ~ t.treatmentArmId ~ t.identifier ~ t.investigatorUserName ~ t.trialSiteId

    private val queryTrialSubjectsFromTreatmentArmId = for {
      treatmentArmId <- Parameters[Int]
      t <- TrialSubjects if t.treatmentArmId === treatmentArmId
    } yield t.id ~ t.version ~ t.createdAt ~ t.treatmentArmId ~ t.identifier ~ t.investigatorUserName ~ t.trialSiteId

    private val queryTrialSubjectsFromTrialId = for {
      trialId <- Parameters[Int]
      arms <- TreatmentArms if arms.trialId === trialId
      t <- TrialSubjects if t.treatmentArmId === (arms.id)
    } yield t.id ~ t.version ~ t.createdAt ~ t.treatmentArmId ~ t.identifier ~ t.investigatorUserName ~ t.trialSiteId

    private val querySubjectPropertiesFromSubjectId = for {
      subjectId <- Parameters[Int]
      properties <- SubjectProperties if properties.subjectId === subjectId
    } yield properties.id ~ properties.version ~ properties.criterionId ~ properties.subjectId ~ properties.dateValue ~ properties.stringValue ~ properties.intValue ~ properties.doubleValue

    private val queryTrialIdFormSubjectId = for {
      subjectId <- Parameters[Int]
      subject <- TrialSubjects if subject.id === subjectId
      arms <- TreatmentArms if arms.id === subject.treatmentArmId
    } yield arms.trialId

    private val queryTrialIdFromTreatmentArmId = for {
      treatmentArmId <- Parameters[Int]
      arms <- TreatmentArms if arms.id === treatmentArmId
    } yield arms.trialId

    def create(trialSubject: TrialSubject, treatmentArmId: Int): Validation[String, Int] = {
      //TODO check unique subject identifier
      onDB {
        threadLocalSession withTransaction {
          TrialSubjects.noId insert(trialSubject.version, new Timestamp(trialSubject.createdAt.getMillis), treatmentArmId, trialSubject.identifier, trialSubject.investigatorUserName, trialSubject.trialSite.id)
        }
        val id = getId(treatmentArmId, trialSubject.identifier).either match {
          case Left(x) => return Failure(x)
          case Right(subjectId) => subjectId
        }
        trialSubject.properties.foreach {
          property =>
            if (property.criterion.getClass == classOf[DateCriterion])
              SubjectProperties.noId insert(0, property.criterion.id, id, Some(new java.sql.Date(property.value.asInstanceOf[java.util.Date].getTime)), None, None, None)
            else if (property.criterion.getClass == classOf[DoubleCriterion])
              SubjectProperties.noId insert(0, property.criterion.id, id, None, None, None, Some(property.value.asInstanceOf[Double]))
            else if (property.criterion.getClass == classOf[IntegerCriterion])
              SubjectProperties.noId insert(0, property.criterion.id, id, None, None, Some(property.value.asInstanceOf[Int]), None)
            else if (property.criterion.getClass == classOf[FreeTextCriterion] || property.criterion.getClass == classOf[OrdinalCriterion])
              SubjectProperties.noId insert(0, property.criterion.id, id, None, Some(property.value.asInstanceOf[String]), None, None)
        }
        Success(id)
      }
    }

    private def getId(treatmentArmId: Int, identifier: String): Validation[String, Int] = {
      for (t <- queryTrialSubjectFromTreatmentArmIdAndIdentifier(treatmentArmId, identifier)) return Success(t._1)
      Failure("Treatment arm with id = " + treatmentArmId + " not found")
    }

    def get(id: Int): Validation[String, Option[TrialSubject]] = {

      onDB {
        //TODO more than one trial
        val trialId = queryTrialIdFormSubjectId(id).list.head
        createOneTrialSubjectFromDatabaseRow(trialId, queryTrialSubjectFromId(id).list)

      }
    }

    private def createOneTrialSubjectFromDatabaseRow(trialId: Int, trialSubjectList: List[(Int, Int, Timestamp, Int, String, String, Int)]): Validation[String, Option[TrialSubject]] = {

      if (trialSubjectList.isEmpty) Success(None)
      else if (trialSubjectList.size > 1) Failure("Dublicate trial subject entry")
      else {
        createTrialSubjectsFromDatabaseRows(trialId, trialSubjectList).either match {
          case Left(x) => Failure(x)
          case Right(resultList) => {
            if (resultList.size == 1) Success(Some(resultList.head))
            else Failure("Could not create trial subject")
          }
        }
      }
    }

    private def createTrialSubjectsFromDatabaseRows(trialId: Int, trialSubjectList: List[(Int, Int, Timestamp, Int, String, String, Int)]): Validation[String, List[TrialSubject]] = {

      if (trialSubjectList.isEmpty) Success(Nil)
      else {
        val results = new ListBuffer[TrialSubject]
        val criterions = criterionDao.getCriterions(trialId).either match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }
        val trialSites = trialSiteDao.getAll.either match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }
        trialSubjectList.foreach {
          subjectRow =>
            val trialSite = trialSites.find(site => site.id == subjectRow._7).getOrElse(return Failure("TrialSite not found"))
            getProperties(subjectRow._1, criterions).either match {
              case Left(x) => return Failure(x)
              case Right(properties) => {
                TrialSubject(subjectRow._1, subjectRow._2, new DateTime(subjectRow._3.getTime), subjectRow._5, subjectRow._6, trialSite, properties, Map()).either match {
                  case Left(x) => return Failure(text("database.entryCorrupt") +" "+ x.toString())
                  case Right(subject) => results += subject
                }

              }
            }
        }
        Success(results.toList)
      }

    }

    private def getProperties(subjectId: Int, criterions: List[Criterion[Any, Constraint[Any]]]): Validation[String, List[SubjectProperty[Any]]] = {
      val properties = new ListBuffer[SubjectProperty[Any]]()
      val resultList = querySubjectPropertiesFromSubjectId(subjectId).list
      for (prop <- querySubjectPropertiesFromSubjectId(subjectId)) {
        val criterion = criterions.find(crit => crit.id == prop._3).getOrElse(return Failure("Criterion not found"))
        properties.append((
          if (criterion.getClass == classOf[DateCriterion])
            SubjectProperty(prop._1, prop._2, criterion, new Date(prop._5.get.getTime))
          else if (criterion.getClass == classOf[DoubleCriterion])
            SubjectProperty(prop._1, prop._2, criterion, prop._8.get)
          else if (criterion.getClass == classOf[IntegerCriterion])
            SubjectProperty(prop._1, prop._2, criterion, prop._7.get)
          else if (criterion.getClass == classOf[FreeTextCriterion] || criterion.getClass == classOf[OrdinalCriterion])
            SubjectProperty(prop._1, prop._2, criterion, prop._6.get)
          else return Failure("Criterion type not found: " + criterion.getClass.getName)
          ).either match {
          case Left(x) => return Failure(text("database.entryCorrupt") +" "+ x.toString())
          case Right(actProp) => actProp
        })

      }
      Success(properties.toList)
    }

    def get(identifier: String, trialId: Int): Validation[String, Option[TrialSubject]] = {
      onDB {
        createOneTrialSubjectFromDatabaseRow(trialId, queryTrialSubjectFromIdentifierAndTrialId(identifier, trialId).list)
      }
    }

    def update(trialSubject: TrialSubject): Validation[String, TrialSubject] = {
      onDB {
        queryTrialSubjectFromId(trialSubject.id).mutate {
          r =>
            r.row = r.row.copy(_5 = trialSubject.identifier, _6 = trialSubject.investigatorUserName)
        }
        get(trialSubject.id).either match {
          case Right(Some(subject)) => Success(subject)
          case _ => Failure("Subject not found")
        }
      }
    }

    def getAllFromTreatmentArm(treatmentArmId: Int): Validation[String, List[TrialSubject]] = {
      onDB {
        val trialId = {
          val trialList = queryTrialIdFromTreatmentArmId(treatmentArmId).list
          if (trialList.isEmpty) return Failure("Trial to treatmen arm (id=" + treatmentArmId + ") not found")
          else if (trialList.size > 1) return Failure("More than one Trial to treatmen arm (id=" + treatmentArmId + ") found")
          else trialList(0)
        }
        createTrialSubjectsFromDatabaseRows(trialId, queryTrialSubjectsFromTreatmentArmId(treatmentArmId).list)
      }
    }

    def getAllFromTrial(trialId: Int): Validation[String, List[TrialSubject]] = {
      onDB {
        createTrialSubjectsFromDatabaseRows(trialId, queryTrialSubjectsFromTrialId(trialId).list)
      }
    }

  }

}
