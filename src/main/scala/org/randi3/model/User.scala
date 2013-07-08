package org.randi3.model

import Entity._
import scalaz._
import Scalaz._
import java.util.Locale
import org.apache.commons.codec.digest.DigestUtils
import org.joda.time.{LocalDate, DateTime}

case class User private(id: Int, version: Int, username: String, password: String, email: String, firstName: String, lastName: String, phoneNumber: String, site: TrialSite, rights: Set[TrialRight], administrator: Boolean, canCreateTrial: Boolean, locale: Locale, isActive: Boolean, numberOfFailedLogins: Int, lockedUntil: Option[DateTime], passwordExpiresAt: Option[LocalDate],private val dummy: Any) extends Entity {

}

object User {

  /*
  * use not the hashed password, the hashing happens automatically
  * */
  def apply(id: Int = Int.MinValue, version: Int = 0, username: String, password: String, email: String, firstName: String, lastName: String, phoneNumber: String, site: TrialSite, rights: Set[TrialRight], administrator: Boolean = false, canCreateTrial: Boolean = false, locale: Locale = Locale.ENGLISH, isActive: Boolean = true, numberOfFailedLogins: Int = 0, lockedUntil: Option[DateTime] = None, passwordExpiresAt: Option[LocalDate] = None): ValidationNel[String, User] = {
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
      checkNotNull(isActive),
      checkNotNull(numberOfFailedLogins),
      checkNotNull(lockedUntil),
      checkNotNull(passwordExpiresAt)
    ).toEither match {
      case Left(x) => Failure(x)
      case Right(_) => Success(new User(id, version, username, hashPassword(username, password), email, firstName, lastName, phoneNumber, site, rights, administrator, canCreateTrial, locale, isActive, numberOfFailedLogins, lockedUntil, passwordExpiresAt, null))
    }
  }

  private def validUser = new User(Int.MinValue, 0, "validName", "validPassword", "valid@mail.de", "validFirst", "validLastName", "123456", TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "validPassord", true).toOption.get, Set(), false, false, Locale.ENGLISH, true, 0, None, None, null)

  def check(id: Int = validUser.id, version: Int = validUser.version, username: String = validUser.username, password: String = validUser.password, email: String = validUser.email, firstName: String = validUser.firstName, lastName: String = validUser.lastName, phoneNumber: String = validUser.phoneNumber, site: TrialSite = validUser.site, rights: Set[TrialRight] = validUser.rights, administrator: Boolean = validUser.administrator, canCreateTrial: Boolean = validUser.canCreateTrial, locale: Locale = validUser.locale, isActive: Boolean = validUser.isActive, numberOfFailedLogins: Int = validUser.numberOfFailedLogins, lockedUntil: Option[DateTime] = validUser.lockedUntil, passwordExpiresAt: Option[LocalDate] = validUser.passwordExpiresAt): ValidationNel[String, Boolean] = {
    apply(id, version, username, password, email, firstName, lastName, phoneNumber, site, rights, administrator, canCreateTrial, locale).toEither match {
      case Left(a) => Failure(a)
      case Right(_) => Success(true)
    }
  }

  def hashPassword(user: User, password: String): ValidationNel[String, String] = {
    check(password = password).toEither match {
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