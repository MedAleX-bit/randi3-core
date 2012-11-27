package org.randi3.utility

import org.scalatest.FunSpec
import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.randi3.model.TrialSubject


class MailSenderSpec extends FunSpec with MustMatchers with ShouldMatchers {

  val mailSender = TestingEnvironment.mailSender

  describe("The MailSender sendMessage method") {

    it("should send a mail") {
     //TODO test mail sender
     // mailSender.sendMessage("mail@examle.com", "", "", "TestMail", "Test")

    }
  }


}
