package org.randi3.model

import org.junit.runner.RunWith

import org.scalatest.matchers.MustMatchers

import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import java.util.Locale

@RunWith(classOf[JUnitRunner])
class UserSpec extends FunSpec with MustMatchers {

   describe("The user object apply method") {

    it("should be able to create a valid user object") {
      User(Int.MinValue, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord", true).toOption.get, Set(), false, false, Locale.ENGLISH).toOption match {
        case Some(x) =>
        case None =>  fail("shoud generate a new user object")
      }
    }

    it("should check the id field") {
      User(-1, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord", true).toOption.get, Set(), false, false, Locale.ENGLISH).toEither match {
        case Left(a) => a.list.size must be (1)
        case Right(b) => fail("Id not checked")
      }
    }


  }

}
