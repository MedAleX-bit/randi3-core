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
import scala.slick.lifted.Parameters
import scala.slick.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.utility.{I18NComponent, UtilityDBComponent}
import java.sql.Timestamp
import org.joda.time.{LocalDate, DateTime}
import scala.collection.mutable

trait TrialSubjectDaoComponent {

  this: DaoComponent with
    CriterionDaoComponent with
    TrialSiteDaoComponent with
    UtilityDBComponent with
    I18NComponent =>

  val trialSubjectDao: TrialSubjectDao

  class TrialSubjectDao {

    import driver.Implicit._
    import schema._
    import utilityDB._
    import i18n._

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
      properties <- SubjectProperties if properties.subjectId === subjectId && properties.stageName.isNull
    } yield properties.id ~ properties.version ~ properties.criterionId ~ properties.subjectId ~ properties.dateValue ~ properties.stringValue ~ properties.intValue ~ properties.doubleValue

    private val querySubjectStagePropertiesFromSubjectId = for {
      subjectId <- Parameters[Int]
      properties <- SubjectProperties if properties.subjectId === subjectId && properties.stageName.isNotNull
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
        val id = getId(treatmentArmId, trialSubject.identifier).toEither match {
          case Left(x) => return Failure(x)
          case Right(subjectId) => subjectId
        }
        trialSubject.properties.foreach {  property =>  saveProperty(id, property) }
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
      else if (trialSubjectList.size > 1) Failure("Duplicated trial subject entry")
      else {
        createTrialSubjectsFromDatabaseRows(trialId, trialSubjectList).toEither match {
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
        val criterions = criterionDao.getCriterions(trialId).toEither match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }

        val stages = criterionDao.getStages(trialId).toEither match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }
        val trialSites = trialSiteDao.getAll.toEither match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }
        trialSubjectList.foreach {
          subjectRow =>
            val trialSite = trialSites.find(site => site.id == subjectRow._7).getOrElse(return Failure("TrialSite not found"))
            getProperties(subjectRow._1, criterions).toEither match {
              case Left(x) => return Failure(x)
              case Right(properties) => {
                getStageProperties(subjectRow._1, stages).toEither match {
                  case Left(x) => Failure(x)
                  case Right(stageProperties) => {
                    TrialSubject(subjectRow._1, subjectRow._2, new DateTime(subjectRow._3.getTime), subjectRow._5, subjectRow._6, trialSite, properties, stageProperties).toEither match {
                      case Left(x) => return Failure(text("database.entryCorrupt") +" "+ x.toString())
                      case Right(subject) => results += subject
                    }
                  }
                }
              }
            }
        }
        Success(results.toList)
      }

    }

    private def saveProperty(subjectId: Int, property: SubjectProperty[_], stageName: Option[String] = None) {

      if (property.criterion.getClass == classOf[DateCriterion])
        SubjectProperties.noId insert(0, property.criterion.id, subjectId, Some(new java.sql.Date(property.value.asInstanceOf[LocalDate].toDate.getTime)), None, None, None, stageName)
      else if (property.criterion.getClass == classOf[DoubleCriterion])
        SubjectProperties.noId insert(0, property.criterion.id, subjectId, None, None, None, Some(property.value.asInstanceOf[Double]), stageName)
      else if (property.criterion.getClass == classOf[IntegerCriterion])
        SubjectProperties.noId insert(0, property.criterion.id, subjectId, None, None, Some(property.value.asInstanceOf[Int]), None, stageName)
      else if (property.criterion.getClass == classOf[FreeTextCriterion] || property.criterion.getClass == classOf[OrdinalCriterion])
        SubjectProperties.noId insert(0, property.criterion.id, subjectId, None, Some(property.value.asInstanceOf[String]), None, None, stageName)
    }

    private def getProperties(subjectId: Int, criterions: List[Criterion[Any, Constraint[Any]]]): Validation[String, List[SubjectProperty[Any]]] = {
      val properties = new ListBuffer[SubjectProperty[Any]]()

     for (prop <- querySubjectPropertiesFromSubjectId(subjectId)) {
        val criterion = criterions.find(crit => crit.id == prop._3).getOrElse(return Failure("Criterion not found"))
        properties.append((
          if (criterion.getClass == classOf[DateCriterion])
            SubjectProperty(prop._1, prop._2, criterion, new LocalDate(prop._5.get.getTime))
          else if (criterion.getClass == classOf[DoubleCriterion])
            SubjectProperty(prop._1, prop._2, criterion, prop._8.get)
          else if (criterion.getClass == classOf[IntegerCriterion])
            SubjectProperty(prop._1, prop._2, criterion, prop._7.get)
          else if (criterion.getClass == classOf[FreeTextCriterion] || criterion.getClass == classOf[OrdinalCriterion])
            SubjectProperty(prop._1, prop._2, criterion, prop._6.get)
          else return Failure("Criterion type not found: " + criterion.getClass.getName)
          ).toEither match {
          case Left(x) => return Failure(text("database.entryCorrupt") +" "+ x.toString())
          case Right(actProp) => actProp
        })

      }
      Success(properties.toList)
    }

    private def getStageProperties(subjectId: Int, stages: Map[String, List[Criterion[Any, Constraint[Any]]]]): Validation[String, Map[String, List[SubjectProperty[Any]]]] = {
      val stageProperties = new mutable.HashMap[String, ListBuffer[SubjectProperty[Any]]]()

      val propertyList = querySubjectStagePropertiesFromSubjectId(subjectId).list()

      stages.foreach(stage => {
        stageProperties.put(stage._1, new ListBuffer())

        stage._2.foreach(criterion => {
          val property = propertyList.find(_._3 == criterion.id)

          property match {
            case None =>  //nothing to do -> a stage property can be empty
            case Some(prop) => {
              val properties = stageProperties.get(stage._1).getOrElse(return Failure("Stages not correct initialized"))
              properties.append((
                if (criterion.getClass == classOf[DateCriterion])
                  SubjectProperty(prop._1, prop._2, criterion, new LocalDate(prop._5.get.getTime))
                else if (criterion.getClass == classOf[DoubleCriterion])
                  SubjectProperty(prop._1, prop._2, criterion, prop._8.get)
                else if (criterion.getClass == classOf[IntegerCriterion])
                  SubjectProperty(prop._1, prop._2, criterion, prop._7.get)
                else if (criterion.getClass == classOf[FreeTextCriterion] || criterion.getClass == classOf[OrdinalCriterion])
                  SubjectProperty(prop._1, prop._2, criterion, prop._6.get)
                else return Failure("Criterion type not found: " + criterion.getClass.getName)
                ).toEither match {
                case Left(x) => return Failure(text("database.entryCorrupt") +" "+ x.toString())
                case Right(actProp) => actProp
              })
            }
          }
        })
      })

      Success(stageProperties.map(entry => entry._1 -> entry._2.toList).toMap)
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
        get(trialSubject.id).toEither match {
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

    def addStage(trialSubject: TrialSubject, stageName: String, properties: List[SubjectProperty[_ <: Any]]) : Validation[String, TrialSubject] = {
      onDB {
        threadLocalSession withTransaction {
          properties.foreach(property =>
            saveProperty(trialSubject.id, property, Some(stageName))
          )
          get(trialSubject.id).toEither match {
            case Right(Some(subject)) => Success(subject)
            case _ => Failure("Subject not found")
          }
        }
      }

    }

  }

}
