package org.randi3.utility

import org.scalatest.Spec
import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.randi3.model.TrialSubject


class MailSenderSpec extends Spec with MustMatchers with ShouldMatchers {

  val mailSender = TestingEnvironment.mailSender

  describe("The MailSender sendMessage method") {

    it("should send a mail") {
     //TODO test mail sender
     // mailSender.sendMessage("mail@examle.com", "", "", "TestMail", "Test")

    }
  }


}
