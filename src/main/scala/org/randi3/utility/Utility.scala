package org.randi3.utility

import scalaz._
import java.sql.SQLException
import org.randi3.dao._
import org.joda.time.DateTime
import org.randi3.configuration.{ ConfigurationValues, ConfigurationServiceComponent }

trait Utility extends Logging {

  def logError(t: Throwable): String = {
    logger.error(t.getMessage, t)
    t.getMessage
  }

  def logError(t: String): String = {
    logger.error(t)
    t
  }

}

import org.randi3.model._
import org.stringtemplate.v4._

trait UtilityDBComponent extends Utility {

  this: DaoComponent with AuditDaoComponent =>

  val utilityDB: UtilityDB

  class UtilityDB {

    def onDB[A](code: => Validation[String, A]): Validation[String, A] = {
      try {
        database withSession {
          code
        }
      } catch {
        case e: SQLException =>
          logError(e); Failure("Database error: " + e.getMessage)
        case e: Throwable => Failure(logError(e))
      }
    }

    def logAudit(user: User, action: ActionType.Value, entity: Entity, text: String): Validation[String, Boolean] = {
      logAudit(user, action, entity, entity.id, text)
    }

    def logAudit(user: User, action: ActionType.Value, entity: Entity, identifier: Int, text: String): Validation[String, Boolean] = {
      logger.info(user.username + ": " + action.toString + " - " + entity.getClass.getSimpleName + " (" + identifier + ") Content: " + text)
      auditDao.create(new AuditEntry(time = new DateTime(), username = user.username, action = action, clazz = entity.getClass.asInstanceOf[Class[Any]], identifier = identifier, text = text))
    }

  }

}

trait UtilityMailComponent extends Utility {

  this: SecurityComponent with UserDaoComponent with ConfigurationServiceComponent with I18NComponent =>

  val utilityMail: UtilityMail

  class UtilityMail {

    import i18n._

    def getRandomizedMailContent(trial: Trial, treatmentArm: TreatmentArm, trialSubject: TrialSubject): String = {

      val st = new ST(getMail("mail.randomized", securityUtility.currentUser), '$', '$')

      st.add("identifier", trialSubject.identifier)
      st.add("trialName", trial.name)
      st.add("treatmentArm", treatmentArm.name)
      st.add("properties", if (!trialSubject.properties.isEmpty) trialSubject.properties.map(prop => prop.criterion.name + "=" + prop.value.toString + "<br />").reduce((acc, string) => acc + string) else "")

      st.render
    }

    def getRegisteredMailContent(user: User): String = {

      val st = new ST(getMail("mail.registered", Some(user)), '$', '$')

      st.add("url", configurationService.getConfigurationEntry(ConfigurationValues.SERVER_URL.toString).toOption.getOrElse("URL"))
      st.add("username", user.username)
      //TODO Details
      st.add("personalDetails", "Details")

      st.render
    }

    /*
    * returns a mail template string with value
    *
    */
    private def getMail(mailType: String, user: Option[User]): String = {

      val source = scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("mail/mail.st"))
      val lines = source.mkString
      source.close()

      val st = new ST(lines, '$', '$')

      st.add("mailGreetings", text("mail.greetings"))
      st.add("username", if (user.isDefined) (user.get.firstName + " " + user.get.lastName) else "")
      st.add("mailContent", text(mailType))
      st.add("mailSystemMail", text("mail.systemMail"))
      st.add("mailRandi2", text("mail.randi2"))

      st.render
    }

    def getRandomizedMailCCAddresses(trial: Trial): String = {
      userDao.getUsersFromTrial(trial.id).toEither match {
        case Left(x) => ""
        case Right(users) => {

          val mails = users.filter(user => user.isActive).filter(user => {
            val rights = user.rights.filter(right => right.trial.id == trial.id).map(right => right.role)
            rights.contains(Role.principleInvestigator) || rights.contains(Role.trialAdministrator) || rights.contains(Role.monitor)
          }).map(user => user.email)
          if (mails.isEmpty) {
            ""
          } else if (mails.size == 1) {
            mails.head
          } else {
            mails.reduce((acc, mail) => acc + ", " + mail)
          }
        }
      }
    }

    def getRegistrationMailCCAddresses: String = {
      userDao.getAll.toEither match {
        case Left(x) => ""
        case Right(users) => {

          val mails = users.filter(user => {
            user.administrator && user.isActive
          }).map(user => user.email)
          if (mails.isEmpty) {
            ""
          } else if (mails.size == 1) {
            mails.head
          } else {
            mails.reduce((acc, mail) => acc + ", " + mail)
          }
        }
      }
    }
  }

}