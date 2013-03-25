package org.randi3.dao

import java.sql.Date

import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import org.randi3.randomization.RandomizationMethod
import org.scalaquery.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.utility.UtilityDBComponent
import scala.Int
import org.joda.time.LocalDate
import org.randi3.model._
import scala.Left
import scalaz.Failure
import scala.Right
import scala.Some
import scalaz.Success
import org.scalaquery.ql.Parameters
import org.scalaquery.ql.Query

trait TrialDaoComponent {

  this: DaoComponent with
    TrialSiteDaoComponent with
    TreatmentArmDaoComponent with
    RandomizationMethodDaoComponent with
    CriterionDaoComponent with
    UtilityDBComponent =>

  val trialDao: TrialDao

  class TrialDao {

    import driver.Implicit._
    import schema._
    import utilityDB._

    private val queryTrialFromId = for {
      id <- Parameters[Int]
      t <- Trials if t.id is id
    } yield t


    private val queryParitcipationSitestFromTrialId = for {
      id <- Parameters[Int]
      ps <- ParticipatingSites if ps.trialId is id
    } yield ps.trialId ~ ps.trialSiteId


    private val queryTrialFromName = for {
      name <- Parameters[String]
      t <- Trials if t.name is name
    } yield t

    private def allTrials = database withSession {
      Query(Trials).list
    }

    def create(trial: Trial): Validation[String, Int] = {
      onDB {
        val id = {
          threadLocalSession withTransaction {
            Trials.noId insert(
              trial.version,
              trial.name,
              trial.abbreviation,
              trial.description,
              new Date(trial.startDate.toDate.getTime),
              new Date(trial.endDate.toDate.getTime),
              trial.status.toString,
              trial.identificationCreationType.toString,
              trial.isEDCTrial,
              trial.isTrialOpen,
              trial.isStratifiedByTrialSite)
          }
          getId(trial.name).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }
        }

        threadLocalSession withTransaction {
          ParticipatingSites insertAll (trial.participatingSites.map(site => (id, site.id)).toSeq: _*)
        }

        if (trial.randomizationMethod.isDefined) {
          randomizationMethodDao.create(trial.randomizationMethod.get, id)
        }


        if (trial.treatmentArms != null)
          saveTreatmentArms(trial.treatmentArms, id).either match {
            case Left(x) => return Failure(x)
            case _ =>
          }

        //TODO failure handling
        if (trial.criterions != null)
          saveCriterions(trial.criterions.asInstanceOf[List[Criterion[Any, Constraint[Any]]]], id)

        //TODO failure handling
        saveStages(trial.stages.asInstanceOf[Map[String, List[Criterion[Any, Constraint[Any]]]]], id)

        Success(id)
      }
    }

    private def getId(trialName: String): Validation[String, Int] = {
      for (t <- queryTrialFromName(trialName)) return Success(t._1)
      Failure("Trial not found")
    }

    private def saveTreatmentArms(treatmentArms: List[TreatmentArm], trialId: Int): Validation[String, Boolean] = {
      treatmentArms.foreach {
        arm =>
          treatmentArmDao.create(arm, trialId).either match {
            case Left(x) => return Failure(x)
            case _ =>
          }
      }
      Success(true)
    }

    private def saveCriterions(criterions: List[Criterion[Any, Constraint[Any]]], trialId: Int): Validation[String, Boolean] = {
      for (criterion <- criterions) {
        criterionDao.create(criterion, trialId).either match {
          case Left(x) => return Failure(x)
          case _ =>
        }
      }
      Success(true)
    }

    private def saveStages(trialStages: Map[String, List[Criterion[Any, Constraint[Any]]]], trialId: Int): Validation[String, Boolean] = {
      trialStages.foreach {
        stage =>
          val criterionIds = new ListBuffer[Int]()
          //save the criterions from one stage
          stage._2.foreach {
            criterion =>
              criterionDao.create(criterion, trialId).either match {
                case Left(x) => return Failure(x)
                case Right(id) => criterionIds.append(id)
              }
          }
          //save the stage mapping
          criterionIds.foreach {
            criterionId =>
              TrialStages.noId insert(trialId, stage._1, criterionId)
          }
      }
      Success(true)
    }

    /**
     * returns a the specific trial, with the randomization method and the treatment arms
     */
    def get(id: Int): Validation[String, Option[Trial]] = {
      onDB {
        val trials = queryTrialFromId(id).list
        if (trials.isEmpty) Success(None)
        else if (trials.size > 1) Failure("Duplicated trial id")
        else {
          val t = trials(0)
          val treatmentArms = treatmentArmDao.getAllTreatmentArmsFromTrial(t._1).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }
          val criterions = criterionDao.getCriterions(t._1).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }
          val randomizationMethod = randomizationMethodDao.getFromTrialId(t._1).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }
          val partSites = trialSiteDao.getParticipationSites(t._1).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }

          val stages = criterionDao.getStages(t._1).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }
          Trial(
             id = t._1,
            version = t._2,
            name = t._3,
            abbreviation = t._4,
            description = t._5,
            startDate = new LocalDate(t._6.getTime),
            endDate = new LocalDate(t._7.getTime),
            status = TrialStatus.withName(t._8),
            treatmentArms =  treatmentArms,
            criterions = criterions,
            participatingSites = partSites,
            randomizationMethod = randomizationMethod,
            stages =stages,
            identificationCreationType = TrialSubjectIdentificationCreationType.withName(t._9),
            isEDCTrial = t._10,
            isTrialOpen = t._11,
            isStratifiedByTrialSite = t._12
          ).either match {
            case Left(x) => Failure(x.toString())
            case Right(trial) => Success(Some(trial))
          }

        }
      }
    }

    /**
     * changed the name and the stage count of a trial
     */
    def update(trial: Trial): Validation[String, Trial] = {
      onDB {
        threadLocalSession withTransaction {
        queryTrialFromId(trial.id).mutate {
          r =>
            r.row = r.row.copy(
              _2 = trial.version,
              _3 = trial.name,
              _4 = trial.abbreviation,
              _5 = trial.description,
              _6 = new Date(trial.startDate.toDate.getTime),
              _7 = new Date(trial.endDate.toDate.getTime),
              _8 = trial.status.toString,
              _9 = trial.identificationCreationType.toString,
              _10 = trial.isEDCTrial,
              _11 = trial.isTrialOpen,
              _12 = trial.isStratifiedByTrialSite)
        }
        queryParitcipationSitestFromTrialId(trial.id).mutate( {
              r => r.delete()
          })
          ParticipatingSites insertAll (trial.participatingSites.map(site => (trial.id, site.id)).toSeq: _*)
        }

        val newArms = trial.treatmentArms.filter(_.id == Int.MinValue)

        val changedArms = trial.treatmentArms.filter(_.id != Int.MinValue)

        saveTreatmentArms(newArms, trial.id)

        changedArms.foreach(arm => {
          treatmentArmDao.update(arm)
        })

        val newCriterions = trial.criterions.filter(_.id == Int.MinValue)

        val changedCriterions = trial.criterions.filter(_.id != Int.MinValue)

        saveCriterions(newCriterions.asInstanceOf[List[Criterion[Any,Constraint[Any]]]], trial.id)

        changedCriterions.foreach(criterion => {

          criterionDao.update(criterion.asInstanceOf[Criterion[Any,Constraint[Any]]])
        })

        get(trial.id).either match {
          case Right(Some(trialUpdated)) => Success(trialUpdated)
          case _ => Failure("trial not found")
        }
      }
    }


    def updateStatus(trial: Trial): Validation[String, Trial] = {
      onDB {
        threadLocalSession withTransaction {
          queryTrialFromId(trial.id).mutate {
            r =>
              r.row = r.row.copy(_2 = trial.version,  _8 = trial.status.toString)
          }
        }
        get(trial.id).either match {
          case Right(Some(trialUpdated)) => Success(trialUpdated)
          case _ => Failure("trial not found")
        }
      }
    }


    def addRandomizationMethod(trialWithNewMethod: Trial, randomizationMethod: RandomizationMethod): Validation[String, Trial] = {
      onDB {
        randomizationMethodDao.create(randomizationMethod, trialWithNewMethod.id).either match {
          case Left(x) => Failure(x)
          case Right(rId) => get(trialWithNewMethod.id).either match {
            case Right(Some(trial)) => Success(trial)
            case _ => Failure("Trial not found")
          }
        }
      }
    }

    def getAll: Validation[String, List[Trial]] = {
      onDB {
        val resultList: ListBuffer[Trial] = new ListBuffer()
        allTrials.foreach(t =>  {
        val partSites = trialSiteDao.getParticipationSites(t._1).either match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }
          Trial(
            id = t._1,
            version = t._2,
            name = t._3,
            abbreviation = t._4,
            description = t._5,
            startDate = new LocalDate(t._6.getTime),
            endDate = new LocalDate(t._7.getTime),
            status = TrialStatus.withName(t._8),
            treatmentArms =  Nil,
            criterions = Nil,
            participatingSites = partSites,
            randomizationMethod = None,
            stages = Map(),
            identificationCreationType = TrialSubjectIdentificationCreationType.withName(t._9),
            isEDCTrial = t._10,
            isTrialOpen = t._11,
            isStratifiedByTrialSite = t._12
          ).either match {
            case Left(x) => return Failure(x.toString())
            case Right(trial) => resultList += trial
          }
        })
        Success(resultList.toList)
      }
    }

    def delete(trial: Trial) {
      database withSession {
        queryTrialFromId(trial.id).mutate {
          r =>
            r.delete()
        }
      }
    }

  }

}
