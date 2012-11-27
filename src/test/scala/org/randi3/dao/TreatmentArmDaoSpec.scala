package org.randi3.dao


import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.extended.H2Driver.Implicit._

import org.scalaquery.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalaquery.ql.Query

@RunWith(classOf[JUnitRunner])
class TreatmentArmDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._
  import schema._

  describe("The TreatmentArmDao create method") {

    it("should be able to create a treatment arm with a name, a corresponding trial and the planned size") {
      val trialId: Int = trialDao.create(createTrial.copy(treatmentArms = Nil)).toOption.get

      val treatmentArmCount = database withSession {
        Query(TreatmentArms).list.size
      }

      val treatmentArm = createTreatmentArm
      val id = treatmentArmDao.create(treatmentArm, trialId).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      id must be > Int.MinValue

      database withSession {
        val allTreatmentArms = Query(TreatmentArms).list
        allTreatmentArms.size must be(treatmentArmCount + 1)

        val treatmentArmDB = allTreatmentArms.filter(arm => arm._1 == id).head
        treatmentArmDB._2 must be(treatmentArm.version)
        treatmentArmDB._3 must be(treatmentArm.name)
        treatmentArmDB._4 must be(treatmentArm.description)
        treatmentArmDB._5 must be(trialId)
        treatmentArmDB._6 must be(treatmentArm.plannedSize)
      }
    }

  }

  describe("The TreatmentArmDao get method") {

    it("should be able to get a treatment arm with a specific id, including the trial subjects") {
      val trialId: Int = trialDao.create(createTrial.copy(treatmentArms = Nil)).toOption.get

      val treatmentArm = createTreatmentArm
      val id = treatmentArmDao.create(treatmentArm, trialId).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val treatmentArmDB = treatmentArmDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("Arm not found")
        case Right(Some(x)) => x
      }

      treatmentArmDB.id must be(id)
      treatmentArmDB.version must be(treatmentArm.version)
      treatmentArmDB.name must be(treatmentArm.name)
      treatmentArmDB.description must be(treatmentArm.description)
      treatmentArmDB.plannedSize must be(treatmentArm.plannedSize)
      //TODO TrialSubjects
    }

  }

  describe("The TreatmentArmDao getAllTreatmentArmsFromTrial method") {

    it("should be return all treatment arms from a specific trial, without the trial subjects (empty list)") {
      val trialId = trialDao.create(createTrial.copy(treatmentArms = Nil)).toOption.get
      val treatmentArm1 = createTreatmentArm
      val treatmentArm2 = createTreatmentArm

      val id_Arm1 = treatmentArmDao.create(treatmentArm1, trialId).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val id_Arm2 = treatmentArmDao.create(treatmentArm2, trialId).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val treatmentArmsDB = treatmentArmDao.getAllTreatmentArmsFromTrial(trialId).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      treatmentArmsDB.size must be(2)

      treatmentArmsDB.exists(t => t.id == id_Arm1) must be(true)
      treatmentArmsDB.exists(t => t.id == id_Arm2) must be(true)

      val arm1DB = treatmentArmsDB.filter(t => t.id == id_Arm1).head
      arm1DB.name must be(treatmentArm1.name)
      arm1DB.description must be(treatmentArm1.description)
      arm1DB.plannedSize must be(treatmentArm1.plannedSize)
      arm1DB.subjects must not be (null)
      arm1DB.subjects must be('Empty)

      //TODO test with trial subjects

      val arm2DB = treatmentArmsDB.filter(t => t.id == id_Arm2).head
      arm2DB.name must be(treatmentArm2.name)
      arm2DB.description must be(treatmentArm2.description)
      arm2DB.plannedSize must be(treatmentArm2.plannedSize)
      arm2DB.subjects must not be (null)
      arm2DB.subjects must be('Empty)
    }

    it("should be return all treatment arms from a specific trial, with the trial subjects")(pending)
  }

  describe("The TreatmentArmDao update method") {

    it("should be able to update the name") {
      val treatmentArmDB = createTreatmentArmDB

      val changedTreatmentArm = treatmentArmDB.copy(name = "changedName")

      treatmentArmDao.update(changedTreatmentArm)

      val changedTreatmentArmDB = treatmentArmDao.get(treatmentArmDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("Arm not found")
        case Right(Some(x)) => x
      }

      changedTreatmentArmDB.id must be(treatmentArmDB.id)
      changedTreatmentArmDB.name must be(changedTreatmentArm.name)
      changedTreatmentArmDB.description must be(treatmentArmDB.description)
      changedTreatmentArmDB.plannedSize must be(treatmentArmDB.plannedSize)
    }

    it("should be able to update the description") {
      val treatmentArmDB = createTreatmentArmDB

      val changedTreatmentArm = treatmentArmDB.copy(description = "description 2")

      treatmentArmDao.update(changedTreatmentArm)

      val changedTreatmentArmDB = treatmentArmDao.get(treatmentArmDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("Arm not found")
        case Right(Some(x)) => x
      }

      changedTreatmentArmDB.id must be(treatmentArmDB.id)
      changedTreatmentArmDB.name must be(treatmentArmDB.name)
      changedTreatmentArmDB.description must be(changedTreatmentArm.description)
      changedTreatmentArmDB.plannedSize must be(treatmentArmDB.plannedSize)
    }

    it("should be able to update the planned subject size") {
      val treatmentArmDB = createTreatmentArmDB

      val changedTreatmentArm = treatmentArmDB.copy(plannedSize = 50)

      treatmentArmDao.update(changedTreatmentArm)

      val changedTreatmentArmDB = treatmentArmDao.get(treatmentArmDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("Arm not found")
        case Right(Some(x)) => x
      }

      changedTreatmentArmDB.id must be(treatmentArmDB.id)
      changedTreatmentArmDB.name must be(treatmentArmDB.name)
      changedTreatmentArmDB.description must be(treatmentArmDB.description)
      changedTreatmentArmDB.plannedSize must be(changedTreatmentArm.plannedSize)
    }

  }

}
