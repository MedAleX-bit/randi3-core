package org.randi3.dao

import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.extended.H2Driver.Implicit._

import org.scalaquery.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model.Trial
import org.apache.commons.math3.random.MersenneTwister

import org.scalaquery.ql.Query

@RunWith(classOf[JUnitRunner])
class TrialDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._
  import schema._

  val randomizationPlugin = randomizationPluginManager.getPlugin("org.randi3.randomization.CompleteRandomization").get

  import randomizationPlugin._

  describe("The TrialDao create method") {

    it("should be able to create a trial without treatment arms and a randomization method") {
      val trialsCount = database withSession {
        Query(Trials).list.size
      }
      val trial: Trial = createTrial.copy(randomizationMethod = None, treatmentArms = Nil)
      val id = trialDao.create(trial).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      database withSession {
        val allTrials = Query(Trials).list
        allTrials.size must be(trialsCount + 1)

        val trials = for {
          t <- allTrials
          if t._1 == id
        } yield t

        trials.size must be(1)
        trials.head._3 must be(trial.name)
        trials.head._4 must be(trial.abbreviation)
        trials.head._5 must be(trial.description)
        trials.head._6.getTime must be(trial.startDate.toDate.getTime)
        trials.head._7.getTime must be(trial.endDate.toDate.getTime)
        trials.head._8 must be(trial.stratifyTrialSite.toString)
      }
    }

    it("should be able to create a trial without treatment arms and with a randomization method") {
      val trialsCount = database withSession {
        Query(Trials).list.size
      }
      val trial: Trial = createTrial.copy(treatmentArms = Nil)
      val id = trialDao.create(trial).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      database withSession {
        val allTrials = Query(Trials).list
        allTrials.size must be(trialsCount + 1)

        val trials = for {
          t <- allTrials
          if t._1 == id
        } yield t

        trials.size must be(1)
        trials.head._3 must be(trial.name)
        trials.head._4 must be(trial.abbreviation)
        trials.head._5 must be(trial.description)
        trials.head._6.getTime must be(trial.startDate.toDate.getTime)
        trials.head._7.getTime must be(trial.endDate.toDate.getTime)
        trials.head._8 must be(trial.stratifyTrialSite.toString)

        val methods = for {
          m <- Query(RandomizationMethods).list
          if m._2 == id
        } yield m

        methods.size must be(1)
        methods.head._1.get must not be (Int.MinValue)
        methods.head._4 must be(name)
      }
    }

    it("should be able to create a trial with treatment arms and a randomization method") {
      val trial: Trial = createTrial

      val id = trialDao.create(trial).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val trialDB = trialDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial not found")
        case Right(Some(x)) => x
      }

      trialDB.id must be > Int.MinValue
      trialDB.name must be(trial.name)
      trialDB.treatmentArms.size must be(trial.treatmentArms.size)
      trialDB.criterions must be('empty)
      trialDB.description must be(trial.description)
      trialDB.randomizationMethod must not be (None)
      trialDB.randomizationMethod.get.id must not be (Int.MinValue)
      //TODO
      //      trialDB.randomizationMethod.random.nextDouble() must be(trial.randomizationMethod.random.nextDouble())
    }
  }

  describe("The TrialDao get method") {

    it("should be able to get a trial without treatment arms and a randomization method") {
      val trial: Trial = createTrial.copy(treatmentArms = Nil)
      val id = trialDao.create(trial).toOption.get

      val trialDB = trialDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial not found")
        case Right(Some(x)) => x
      }

      trialDB.id must be > Int.MinValue
      trialDB.name must be(trial.name)
      trialDB.treatmentArms must be('empty)
      trialDB.criterions must be('empty)
      trialDB.description must be(trial.description)
      trialDB.randomizationMethod must not be (None)
      trialDB.randomizationMethod.get.id must not be (Int.MinValue)
      trialDB.startDate must be(trial.startDate)
      trialDB.endDate must be(trial.endDate)
      //TODO
      //      trialDB.randomizationMethod.random.nextDouble() must be(trial.randomizationMethod.random.nextDouble())
      trialDB.description must be(trial.description)
    }

    it("should be able to get a trial with treatment arms and a randomization method") {
      val trial: Trial = createTrial
      val id = trialDao.create(trial).toOption.get
      val trialDB = trialDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial not found")
        case Right(Some(x)) => x
      }

      trialDB.id must be > Int.MinValue
      trialDB.name must be(trial.name)
      trialDB.treatmentArms.size must be(trial.treatmentArms.size)
      trialDB.criterions must be('empty)
      trialDB.description must be(trial.description)
      trialDB.randomizationMethod must not be (None)
      trialDB.randomizationMethod.get.id must not be (Int.MinValue)
      // TODO    
      //trialDB.randomizationMethod.random.nextDouble() must be(trial.randomizationMethod.random.nextDouble())
      trialDB.description must be(trial.description)
    }

  }

  describe("The TrialDao getAll method") {

    it("should be return all trials, but only with id, name, description and stage count") {

      val actTrialsCount = trialsCount

      trialDao.getAll.either match {
        case Left(x) => fail(x)
        case Right(trials) => trials.size must be(actTrialsCount)
      }

      (1 to 10).foreach(_ => createTrialDB)

      val resultList = trialDao.getAll.either match {
        case Left(x) => fail(x)
        case Right(trials) => trials
      }

      resultList.size must be(actTrialsCount + 10)
      //      resultList.foreach { trial =>
      //        trial.randomizationMethod.isDefined must be(true)
      //        trial.treatmentArms.size must be(2)
      //      }
    }
  }

  describe("The TrialDao update method") {

    it("should be able to update the name") {
      val trialDB = createTrialDB
      val changedTrial = trialDB.copy(name = "trial8")
      trialDao.update(changedTrial)
      trialDao.get(trialDB.id).toOption.get.get.name must be(changedTrial.name)
    }

    it("should be able to update the description") {
      val trialDB = createTrialDB
      val changedTrial = trialDB.copy(description = "description 2")
      trialDao.update(changedTrial)
      trialDao.get(trialDB.id).toOption.get.get.description must be(changedTrial.description)
    }

    it("should not be able to update the randomization method")(pending)

    //really?
    it("should be able to update the treatment arms")(pending)
  }

  describe("The TrialDao addRandomizationMethod method") {

    it("should be able to add a randomization method to a trial without one") {
      val trialDB = trialDao.get(trialDao.create(createTrial.copy(randomizationMethod = None)).toOption.get).toOption.get.get

      trialDB.randomizationMethod must be(None)

      trialDao.addRandomizationMethod(trialDB, randomizationMethod(new MersenneTwister, null, Nil).toOption.get).either match {
        case Left(x) => fail(x)
        case Right(trial) => trial.randomizationMethod.getOrElse(fail("mehtod not found")).id must be > 0
      }

      trialDao.get(trialDB.id).toOption.get.get.randomizationMethod must not be (None)
    }

    it("should be not able to add a randomization method to a trial with an existing randomization method") {
      val trialDB = createTrialDB

      trialDB.randomizationMethod must not be (None)

      trialDao.addRandomizationMethod(trialDB, randomizationMethod(new MersenneTwister, null, Nil).toOption.get).either.isLeft must be(true)

      val newTrial = trialDao.get(trialDB.id).toOption.get.get
      newTrial.randomizationMethod must not be None

      newTrial.randomizationMethod.get must be(trialDB.randomizationMethod.get)
    }
  }

  describe("The TrialDao delete method") {

    it("should be able to delete a trial with treatment arms and a randomization method")(pending)

  }

  private def trialsCount = database withSession {
    Query(Trials).list.size
  }
}
