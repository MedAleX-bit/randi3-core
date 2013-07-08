package org.randi3.model

import scalaz._
import Scalaz._
import org.randi3.model.Entity._
import org.apache.commons.codec.digest.DigestUtils

case class TrialSite private(id: Int, version: Int, name: String, country: String, street: String, postCode: String, city: String, password: String, isActive: Boolean, private val dummy: Any) extends Entity {

}

object TrialSite {

  def apply(id: Int = Int.MinValue, version: Int = 0, name: String, country: String, street: String, postCode: String, city: String, password: String, isActive: Boolean): ValidationNel[String, TrialSite] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(name, 2, maxTextLength),
      checkStringBetween(country, 2, maxTextLength),
      checkStringBetween(street, 2, maxTextLength),
      checkStringBetween(postCode, 2, maxTextLength),
      checkStringBetween(city, 2, maxTextLength),
      checkPassword(password)).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new TrialSite(id, version, name, country, street, postCode, city, hashPassword(name, password), isActive, null))
    }
  }

  private val validSite = new TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", hashPassword("validName", "password"), true, null)

  def check(id: Int = validSite.id, version: Int = validSite.version, name: String = validSite.name, country: String = validSite.country, street: String = validSite.street, postCode: String = validSite.postCode, city: String = validSite.city, password: String = validSite.password, isActive: Boolean = validSite.isActive): ValidationNel[String, Boolean] = {
    apply(id, version, name, country, street, postCode, city, password, isActive).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(true)
    }
  }

  def hashPassword(trialSite: TrialSite, password: String): ValidationNel[String, String] = {
    check(password = password).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => hashPassword(trialSite.name, password).success
    }
  }

  private def hashPassword(name: String, password: String): String = {
    if (password.size == 128) password
    else //TODO salt
      DigestUtils.sha512Hex(name + password + "salt")
  }

}