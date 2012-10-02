package org.randi3.model

import Entity._
import scalaz._
import Scalaz._
import java.util.Locale
import org.apache.commons.codec.digest.DigestUtils

case class User private(id: Int, version: Int, username: String, password: String, email: String, firstName: String, lastName: String, phoneNumber: String, site: TrialSite, rights: Set[TrialRight], administrator: Boolean, canCreateTrial: Boolean, locale: Locale, isActive: Boolean, private val dummy: Any) extends Entity {

}

object User {

  /*
  * use not the hashed password, the hashing happens automatically
  * */
  def apply(id: Int = Int.MinValue, version: Int = 0, username: String, password: String, email: String, firstName: String, lastName: String, phoneNumber: String, site: TrialSite, rights: Set[TrialRight], administrator: Boolean = false, canCreateTrial: Boolean = false, locale: Locale = Locale.ENGLISH, isActive: Boolean = true): ValidationNEL[String, User] = {
    checkAll(
      checkID(id),
      checkVersion(version),
      checkStringBetween(username, 2, maxTextLength),
      checkStringBetween(password, 2, maxTextLength),
      checkStringBetween(email, 2, maxTextLength),
      checkStringBetween(firstName, 2, maxTextLength),
      checkStringBetween(lastName, 2, maxTextLength),
      checkStringBetween(phoneNumber, 2, maxTextLength),
      checkNotNull(site),
      checkNotNull(rights),
      checkNotNull(locale),
      checkNotNull(isActive)
    ).either match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new User(id, version, username, hashPassword(username, password), email, firstName, lastName, phoneNumber, site, rights, administrator, canCreateTrial, locale, isActive,null))
    }
  }

  private def validUser = new User(Int.MinValue, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord").toOption.get, Set(), false, false, Locale.ENGLISH, true, null)

  def check(id: Int = validUser.id, version: Int = validUser.version, username: String = validUser.username, password: String = validUser.password, email: String = validUser.email, firstName: String = validUser.firstName, lastName: String = validUser.lastName, phoneNumber: String = validUser.phoneNumber, site: TrialSite = validUser.site, rights: Set[TrialRight] = validUser.rights, administrator: Boolean = validUser.administrator, canCreateTrial: Boolean = validUser.canCreateTrial, locale: Locale = validUser.locale): ValidationNEL[String, Boolean] = {
    apply(id, version, username, password, email, firstName, lastName, phoneNumber, site, rights, administrator, canCreateTrial, locale).either match {
      case Left(a) => Failure(a)
      case Right(_) => Success(true)
    }
  }

  def hashPassword(user: User, password: String): ValidationNEL[String, String] = {
    check(password = password).either match {
      case Left(x) => Failure(x)
      case Right(_) => hashPassword(user.username, password).success
    }
  }

  private def hashPassword(username: String, password: String): String = {
    if (password.size == 128) password
    else //TODO salt
      DigestUtils.sha512Hex(username + password + "salt")
  }

}