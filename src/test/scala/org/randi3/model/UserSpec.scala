package org.randi3.model

import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.specs.runner.JUnitSuiteRunner
import org.apache.commons.math3.random.MersenneTwister
import java.util.Locale

@RunWith(classOf[JUnitSuiteRunner])
class UserSpec extends Spec with MustMatchers with ShouldMatchers {

  private val validUser = User(Int.MinValue, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord").toOption.get, Set(), false, false, Locale.ENGLISH).toOption.get


  describe("The user object apply method") {

    it("should be able to create a valid user object") {
      User(Int.MinValue, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord").toOption.get, Set(), false, false, Locale.ENGLISH).toOption match {
        case Some(x) =>
        case None =>  fail("shoud generate a new user object")
      }
    }

    it("should check the id field") {
      User(0, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord").toOption.get, Set(), false, false, Locale.ENGLISH).either match {
        case Left(a) => a.list.size should be (1)
        case Right(b) => fail("Id not checked")
      }
    }


  }

}
