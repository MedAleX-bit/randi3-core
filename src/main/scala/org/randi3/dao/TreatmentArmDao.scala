package org.randi3.dao

import org.scalaquery.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import org.randi3.model.TrialSubject
import org.randi3.model.TreatmentArm
import scalaz._
import org.randi3.utility.UtilityDBComponent
import org.scalaquery.ql.Parameters


trait TreatmentArmDaoComponent {

  this: DaoComponent with
    TrialSubjectDaoComponent with
    UtilityDBComponent =>

  val treatmentArmDao: TreatmentArmDao

  class TreatmentArmDao {

    import driver.Implicit._
    import schema._
    import utilityDB._

    private val queryTreatmentArmFromId = for {
      id <- Parameters[Int]
      t <- TreatmentArms if t.id === id
    } yield t.id ~ t.version ~ t.name ~ t.description ~ t.trialId ~ t.plannedSize

    private def queryTreatmentArmFromTrialIdAndName(trialId: Int, name: String) = for {
      t <- TreatmentArms if t.name === name && t.trialId === trialId
    } yield t.id ~ t.version ~ t.name ~ t.description ~ t.trialId ~ t.plannedSize

    private val queryTreatmentArmsFromTrialId = for {
      trialId <- Parameters[Int]
      t <- TreatmentArms if t.trialId === trialId
    } yield t.id ~ t.version ~ t.name ~ t.description ~ t.trialId ~ t.plannedSize

    def create(treatmentArm: TreatmentArm, trialId: Int): Validation[String, Int] = {
      //TODO check unique treatment arm name
      onDB {
        threadLocalSession withTransaction {
          TreatmentArms.noId insert(treatmentArm.version, treatmentArm.name, treatmentArm.description, trialId, treatmentArm.plannedSize)
        }
        getId(trialId, treatmentArm.name)
      }
    }

    private def getId(trialId: Int, armName: String): Validation[String, Int] = {
      for (t <- queryTreatmentArmFromTrialIdAndName(trialId, armName)) return Success(t._1)
      Failure("Treatment arm not found")
    }

    def getAllTreatmentArmsFromTrial(trialId: Int): Validation[String, List[TreatmentArm]] = {

      onDB {
        createTreatmentArmFromDatabaseRows(queryTreatmentArmsFromTrialId(trialId).list)
      }

    }

    def get(id: Int): Validation[String, Option[TreatmentArm]] = {
      onDB {
        val treatmentArmList = queryTreatmentArmFromId(id).list
        if (treatmentArmList.isEmpty) Success(None)
        else if (treatmentArmList.size > 1) Failure("Dublicated treatment arms")
        else {
          createTreatmentArmFromDatabaseRows(treatmentArmList).either match {
            case Left(x) => Failure(x)
            case Right(arms) => Success(Some(arms(0)))
          }
        }
      }
    }

    private def createTreatmentArmFromDatabaseRows(rows: List[(Int, Int, String, String, Int, Int)]): Validation[String, List[TreatmentArm]] = {
      val resultList = new ListBuffer[TreatmentArm]
      rows.foreach {
        t =>
          val subjects = new ListBuffer[TrialSubject]()
          trialSubjectDao.getAllFromTreatmentArm(t._1).either match {
            case Left(x) => return Failure(x)
            case Right(trialSubjects) => trialSubjects.copyToBuffer(subjects)
          }
          TreatmentArm(id = t._1, version = t._2, name = t._3, description = t._4, subjects = subjects, plannedSize = t._6).either match {
            case Left(x) => return Failure("Database entry corrupt: " + x.toString())
            case Right(arm) => resultList += arm
          }
      }
      Success(resultList.toList)
    }

    def update(treatmentArm: TreatmentArm): Validation[String, TreatmentArm] = {
      onDB {
        queryTreatmentArmFromId(treatmentArm.id).mutate {
          r =>
            r.row = r.row.copy(_2 = treatmentArm.version, _3 = treatmentArm.name, _4 = treatmentArm.description, _6 = treatmentArm.plannedSize)
        }
        get(treatmentArm.id).either match {
          case Right(Some(arm)) => Success(arm)
          case _ => Failure("treatment arm not found")
        }
      }
    }

    def delete(treatmentArm: TreatmentArm):  Validation[String, Boolean] = {
      onDB {
        queryTreatmentArmFromId(treatmentArm.id).mutate {
          r => r.delete()
        }
        Success(true)
      }
    }

  }

}
