package org.randi3.service

import scalaz._
import org.randi3.dao.{TrialDaoComponent, TrialRightDaoComponent, UserDaoComponent}
import org.randi3.utility._
import org.randi3.model._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

trait UserServiceComponent {

  this: UserDaoComponent with
    TrialDaoComponent with
    TrialRightDaoComponent with
    UtilityDBComponent with
    UtilityMailComponent with
    MailSenderComponent with
    SecurityComponent with
    I18NComponent=>

  val userService: UserService

  class UserService {

    import securityUtility._
    import utilityDB._
    import i18n._


    def login(username: String, password: String): Validation[String, User] = {
      userDao.get(username).either match {
        case Left(x) => Failure(x)
        case Right(None) => Failure(text("failedLogin"))
        case Right(Some(user)) => {
          if (user.site.isActive) {
            if (user.isActive) {
              val passwordHash = User.hashPassword(user, password).either match {
                case Left(x) => return Failure(x.toString())
                case Right(hash) => hash
              }
              if (user.lockedUntil.isEmpty || user.lockedUntil.get.isBeforeNow) {
                if (user.password == passwordHash) {
                  logAudit(user, ActionType.LOGIN, user, "audit.loggedIn")
                  if (user.numberOfFailedLogins > 0 || user.lockedUntil.isDefined) {
                    userDao.update(user.copy(numberOfFailedLogins = 0, lockedUntil = None))
                  } else {
                    Success(user)
                  }
                }
                else {
                  logAudit(user, ActionType.LOGIN_FAILED, user, "audit.passwordNotCorrect")
                  userDao.update(user.copy(numberOfFailedLogins = user.numberOfFailedLogins + 1, lockedUntil = if ((user.numberOfFailedLogins + 1) >= 3) Some(DateTime.now().plusMinutes(15)) else user.lockedUntil))
                  Failure(text("failedLogin"))
                }
              }
              else {
                logAudit(user, ActionType.LOGIN_FAILED, user, "audit.userIsLocked")
                Failure(text("userLocked")+" "+ DateTimeFormat.forPattern("HH:mm:ss").print(user.lockedUntil.get))
              }
            }
            else {
              logAudit(user, ActionType.LOGIN_FAILED, user, "audit.userIsNotActive")
              Failure(text("failedLogin"))
            }
          } else {
            logAudit(user, ActionType.LOGIN_FAILED, user, "audit.trialSiteIsNotActive")
            Failure(text("failedLogin"))
          }
        }
      }
    }

    def getAll: Validation[String, List[User]] = {
      filterList(userDao.getAll)
    }

    def getAllAdministrators: Validation[String, List[User]] = {
      userDao.getAllAdministrators
    }


    def getAllPossibleFromTrial(trial: Trial): Validation[String, List[User]] = {
      filterList(userDao.getPossibleUsersFromTrial(trial.id))
    }
    def getAllFromTrial(trial: Trial): Validation[String, List[User]] = {
      filterList(userDao.getUsersFromTrial(trial.id))
    }

    def get(id: Int): Validation[String, Option[User]] = {
      filterElement(userDao.get(id))
    }

    def create(user: User): Validation[String, Int] = {
      checkUserCanCreate(user, code = {
        userDao.create _
      })
    }

    def register(user: User, trialSitePassword: String): Validation[String, Int] = {
      val trialSitePasswordHash = TrialSite.hashPassword(user.site, trialSitePassword).either match {
        case Left(x) => return Failure(x.head)
        case Right(hash) => hash
      }
      if (user.site.password == trialSitePasswordHash) {
        checkUserCanRegister(user, code = {
          userDao.create _
        }).either match {
          case Left(x) => Failure(x)
          case Right(id) => {
            //TODO  CC
            mailSender.sendMessage(user.email, utilityMail.getRegistrationMailCCAddresses, "", "[" + user.site.name + "] " + "User registered", utilityMail.getRegisteredMailContent(user))
            Success(id)
          }
        }
      } else Failure("Wrong trial site password")
    }

    def update(user: User): Validation[String, User] = {
      val userDb = userDao.get(user.id).toOption.get.get
      checkUserCanUpdate(user, userDb, code = {
        userDao.update _
      })
    }

    def addTrialRight(userId: Int, right: TrialRight): Validation[String, TrialRight] = {
      val dbTrial = trialDao.get(right.trial.id).toOption.get.get
      val dbUser = userDao.get(userId).toOption.get.get
      checkUserCanAddRight(dbUser, right, dbTrial, code = {
        trialRightDao.addRight _
      })
    }

    def removeTrialRight(userId: Int, right: TrialRight): Validation[String, TrialRight] = {
      val dbTrial = trialDao.get(right.trial.id).toOption.get.get
      val dbUser = userDao.get(userId).toOption.get.get
      checkUserCanRemoveRight(dbUser, right, dbTrial, code = {
        trialRightDao.removeRight _
      })
    }

    def delete(user: User) {

    }
  }

}
