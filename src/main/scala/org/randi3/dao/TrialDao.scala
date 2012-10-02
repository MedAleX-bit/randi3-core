package org.randi3.dao

import java.sql.Date

import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import org.randi3.randomization.RandomizationMethod
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.Parameters
import org.scalaquery.ql.Query
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.TypeMapper._
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.utility.UtilityDBComponent
import scala.Int
import scalaz.Digit._1
import org.joda.time.LocalDate
import org.randi3.model._
import scala.Left
import scalaz.Failure
import scala.Right
import scala.Some
import scalaz.Success

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
    import utilityDB._

    private val queryTrialFromId = for {
      id <- Parameters[Int]
      t <- Trials if t.id is id
    } yield t.id ~ t.version ~ t.name ~ t.abbreviation ~ t.description ~ t.startDate ~ t.endDate ~ t.stratifyTrialSite ~ t.status ~ t.subjectIdentificationCreationType


    private val queryParitcipationSitestFromTrialId = for {
      id <- Parameters[Int]
      ps <- ParticipatingSites if ps.trialId is id
    } yield ps.trialId ~ ps.trialSiteId


    private val queryTrialFromName = for {
      name <- Parameters[String]
      t <- Trials if t.name is name
    } yield t.id ~ t.version ~ t.name ~ t.abbreviation ~ t.description ~ t.startDate ~ t.endDate ~ t.stratifyTrialSite ~ t.status ~ t.subjectIdentificationCreationType

    private def allTrials = database withSession {
      Query(Trials).list
    }

    def create(trial: Trial): Validation[String, Int] = {
      onDB {
        val id = {
          threadLocalSession withTransaction {
            Trials.noId insert(trial.version, trial.name, trial.abbreviation, trial.description, new Date(trial.startDate.toDate.getTime), new Date(trial.endDate.toDate.getTime), trial.stratifyTrialSite.toString, trial.status.toString, trial.identificationCreationType.toString)
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
          Trial(t._1, t._2, t._3, t._4, t._5, new LocalDate(t._6.getTime), new LocalDate(t._7.getTime), StratifiedTrialSite.withName(t._8), TrialStatus.withName(t._9), treatmentArms, criterions, partSites, randomizationMethod, stages, TrialSubjectIdentificationCreationType.withName(t._10)).either match {
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
            r.row = r.row.copy(_2 = trial.version, _3 = trial.name, _4 = trial.abbreviation, _5 = trial.description, _6 = new Date(trial.startDate.toDate.getTime), _7 = new Date(trial.endDate.toDate.getTime), _8 = trial.stratifyTrialSite.toString, _9 = trial.status.toString, _10 = trial.identificationCreationType.toString)
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

        //TODO update criterions

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
        allTrials.foreach(t =>
          Trial(t._1, t._2, t._3, t._4, t._5, new LocalDate(t._6.getTime), new LocalDate(t._7.getTime), StratifiedTrialSite.withName(t._8), TrialStatus.withName(t._9), Nil, Nil, Nil, None, Map(), TrialSubjectIdentificationCreationType.withName(t._10)).either match {
            case Left(x) => return Failure(x.toString())
            case Right(trial) => resultList += trial
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
