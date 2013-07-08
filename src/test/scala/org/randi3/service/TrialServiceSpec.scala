package org.randi3.service

import org.scalatest.matchers.MustMatchers

import org.scalatest.FunSpec
import org.randi3.model.{Role, TrialRight, TrialSubject}

class TrialServiceSpec extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironment._

  describe("The TrialService create method") {

    it("should be able to create a trial if a user with ... is logged in") {

    }
  }

  describe("The TrialService randomize method") {

    it("should be able to randomize ...") {
       val trial = createTrialDB
      val userTmp = createUserDB
     userDao.update(userTmp.copy(site = trial.participatingSites.head))

      trialRightDao.addRight(userTmp.id, TrialRight(Role.investigator, trial).toOption.get)
      val user = userDao.get(userTmp.id).toOption.get.get

      securityUtility.user = user



      val trialSubject = TrialSubject(identifier = "system", investigatorUserName = "username", trialSite = trial.participatingSites(0), properties =  Nil, stages = Map()).toOption.get
      val result = trialService.randomize(trial, trialSubject)
      result.toOption.isDefined must be(true)

    }
  }

}