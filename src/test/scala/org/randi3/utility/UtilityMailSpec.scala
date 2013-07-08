package org.randi3.utility

import org.scalatest.FunSpec
import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.randi3.model.TrialSubject


class UtilityMailSpec extends FunSpec with MustMatchers {

  val utilityMail = TestingEnvironment.utilityMail

  describe("The randomization mail method") {

    it("should return the i18n mail content")  {
      val trial = TestingEnvironment.createTrial
      val treatmentArm = trial.treatmentArms.head
      val trialSubject = TrialSubject(identifier = "abc", investigatorUserName = "username", trialSite = TestingEnvironment.createTrialSite, properties = Nil).toEither match {
        case Left(x) => fail(x.toString())
        case Right(subject) =>  subject
      }
    utilityMail.getRandomizedMailContent(trial, treatmentArm, trialSubject) must not be('Empty)
    }
  }

  describe("The registration mail method") {

    it("should return the i18n mail content")  {
    val user = TestingEnvironment.createUser
      utilityMail.getRegisteredMailContent(user) must not be('Empty)
    }
  }
}
