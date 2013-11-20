package org.randi3.dao

import org.randi3.model._
import scala.slick.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import scala.slick.lifted.{Parameters, Query}

import org.randi3.utility._
import scala.Predef._
import org.joda.time.{LocalDate, DateTime}
import java.sql.{Date, Timestamp}
import scalaz._
import Scalaz._
import java.util.Locale

trait UserDaoComponent {

  this: DaoComponent with
    TrialSiteDaoComponent with
    TrialRightDaoComponent with
    UtilityDBComponent with
   I18NComponent =>

  val userDao: UserDao

  class UserDao {

    import driver.Implicit._
    import schema._
    import utilityDB._
    import i18n._

    private val queryUserFromId = for {
      id <- Parameters[Int]
      user <- Users if user.id is id
    } yield user

    private val queryUserFromUsername = for {
      username <- Parameters[String]
      user <- Users if user.username is username
    } yield user.id

    private val queryUserFromTrialSite = for {
      trialSiteId <- Parameters[Int]
      user <- Users if user.siteId is trialSiteId
    } yield user

    private def queryUserFromTrial(trialId: Int) =  {
      Query(Users).filter(user => user.id in Query(Rights).filter(right => right.trialId is trialId).map(right => right.userId))
    }

    private def queryPossibleUserFromTrial(trialId: Int) =  {
      Query(Users).filter(user => user.siteId in Query(ParticipatingSites).filter(site => site.trialId is trialId).map(psite => psite.trialSiteId))
    }

    def create(user: User): Validation[String, Int] = {
      onDB {
        threadLocalSession withTransaction {
          Users.noId insert(user.version, user.username, user.email, user.firstName,
            user.lastName, user.phoneNumber, user.site.id, user.password,
            user.administrator, user.canCreateTrial, user.isActive,
            user.numberOfFailedLogins,
            if (user.lockedUntil.isDefined) Some(new Timestamp(user.lockedUntil.get.getMillis)) else None,
            if (user.passwordExpiresAt.isDefined) Some(new Date(user.passwordExpiresAt.get.toDate.getTime)) else None,
            user.locale.toString)
        }
        getId(user.username).toEither match {
          case Left(x) => return Failure(x)
          case Right(id) => {

            trialRightDao.createAll(id, user.rights)

            Success(id)
          }

        }


      }

    }

    private def getId(username: String): Validation[String, Int] = {
      //TODO double entry
      for (userId <- queryUserFromUsername(username)) return Success(userId)
      Failure("User not found/saved")
    }

    def get(id: Int): Validation[String, Option[User]] = {
      onDB {
        val result = queryUserFromId(id).list
        if (result.isEmpty) Success(None)

        else if (result.size > 1) Failure("id was not unique")

        else {
          generateUserWithSameTrialSite(result, true, result(0)._8).toEither match {
            case Left(failure) =>  Failure(failure)
            case Right(users) => Success(Some(users.head))
          }
        }
      }
    }

    def get(username: String): Validation[String, Option[User]] = {
      onDB {
        val resultList = queryUserFromUsername(username).list
        if (resultList.isEmpty) Failure("User not found")
        else if (resultList.size == 1) get(resultList.head)
        else Failure("dublicated username")
      }
    }

    def update(user: User): Validation[String, User] = {
      onDB {
        val passwordHash = User.hashPassword(user, user.password).toEither match {
          case Left(x) => return Failure(x.toString())
          case Right(password) => password
        }
        threadLocalSession withTransaction {
          queryUserFromId(user.id).mutate {
            r =>
              r.row = r.row.copy(
                _2 = user.version,
                _4 = user.email,
                _5 = user.firstName,
                _6 = user.lastName,
                _7 = user.phoneNumber,
                _8 = user.site.id,
                _9 = passwordHash,
                _10 = user.administrator,
                _11 = user.canCreateTrial,
                _12 = user.isActive,
              _13 = user.numberOfFailedLogins,
              _14 = if (user.lockedUntil.isDefined) Some(new Timestamp(user.lockedUntil.get.getMillis)) else None,
              _15 = if (user.passwordExpiresAt.isDefined) Some(new Date(user.passwordExpiresAt.get.toDate.getTime)) else None,
              _16 = user.locale.toString)
          }
        }

        trialRightDao.updateRights(user.id, user.rights).toEither match {
          case Left(x) => return Failure(x)
          case Right(_) =>
        }

        get(user.id).toEither match {
          case Left(x) => Failure(x)
          case Right(None) => Failure("user not found")
          case Right(Some(userUpdated)) => Success(userUpdated)
        }
      }
    }

    def getAll: Validation[String, List[User]] = {
      onDB {
        generateUsers(Query(Users).list, false)
      }
    }

    /**
     *
     * @return Failure or an list with the system administrators (only rudimentary infos)
     */
    def getAllAdministrators: Validation[String, List[User]] = {
      onDB {
        generateUsers( Query(Users).filter(row => row.administrator && row.isActive).list, false)
      }
    }


    def getUsersFromTrialSite(trialSiteId: Int): Validation[String, List[User]] = {
      onDB {
        generateUserWithSameTrialSite(queryUserFromTrialSite(trialSiteId).list(), true, trialSiteId)
      }
    }


    def getPossibleUsersFromTrial(trialId: Int): Validation[String, List[User]] = {
      onDB {
        generateUsers(queryPossibleUserFromTrial(trialId).list())
      }
    }

    def getUsersFromTrial(trialId: Int): Validation[String, List[User]] = {
      onDB {
        generateUsers(queryUserFromTrial(trialId).list())
      }
    }

    def deactivateUsersFromTrialSite(trialSiteId: Int): Validation[String, String] = {
     onDB {
        threadLocalSession withTransaction {
          queryUserFromTrialSite(trialSiteId).mutate {
            r =>
              r.row = r.row.copy(_12 = false)
          }
        }
        Success("Users deactivated")
      }
    }



    private def generateUserWithSameTrialSite(userRows: List[(Int, Int, String, String, String, String, String, Int, String, Boolean, Boolean, Boolean, Int, Option[Timestamp], Option[Date], String)], withRights: Boolean = true, trialSiteId: Int): Validation[String, List[User]] = {
      trialSiteDao.get(trialSiteId).toEither match {
        case Left(x) => Failure("Failure by loading the trial site -" + x)
        case Right(None) => Failure("trial site not found")
        case Right(Some(ts)) => generateUsersGivenTrialSiteList(userRows, withRights, List(ts))
      }
    }

    private def generateUsers(userRows: List[(Int, Int, String, String, String, String, String, Int, String, Boolean, Boolean, Boolean, Int, Option[Timestamp], Option[Date], String)], withRights: Boolean = true): Validation[String, List[User]] = {
        trialSiteDao.getAll.toEither match {
        case Left(x) => Failure(x)
        case Right(ts) => generateUsersGivenTrialSiteList(userRows, withRights, ts)
      }

    }

    private def generateUsersGivenTrialSiteList(userRows: List[(Int, Int, String, String, String, String, String, Int, String, Boolean, Boolean, Boolean, Int, Option[Timestamp], Option[Date], String)], withRights: Boolean = true, trialSites: List[TrialSite]): Validation[String, List[User]] = {
      val results = new ListBuffer[User]()
      for (userRow <- userRows) {
        val trialSite = trialSites.find(site => site.id == userRow._8) match {
          case None => return Failure("trial sites not found")
          case Some(ts) => ts
        }
        trialRightDao.getAll(userRow._1).toEither match {
          case Left(x) => Failure(x)
          case Right(trialRights) =>
            generateUserObject(userRow, trialSite, trialRights).toEither match {
              case Left(x) =>  Failure(text("database.entryCorrupt") +" "+ x.toString())
              case Right(user) => results += user
            }
        }
      }
      Success(results.toList)
    }

    private def generateUserObject(userRow: (Int, Int, String, String, String, String, String, Int, String, Boolean, Boolean, Boolean, Int, Option[Timestamp], Option[Date], String), trialSite: TrialSite, trialRights: Set[TrialRight]): ValidationNel[String, User] = {
      User(id = userRow._1, version = userRow._2, username = userRow._3,
        email = userRow._4, firstName = userRow._5, lastName = userRow._6,
        phoneNumber = userRow._7, site = trialSite, password = userRow._9,
        rights = trialRights, administrator = userRow._10, canCreateTrial = userRow._11,
        isActive = userRow._12, numberOfFailedLogins = userRow._13,
        lockedUntil = if (userRow._14.isDefined) Some(new DateTime(userRow._14.get.getTime)) else None,
        passwordExpiresAt = if (userRow._15.isDefined) Some(new LocalDate(userRow._15.get.getTime)) else None,
        locale = Locale.getAvailableLocales.find(loc => loc.toString == userRow._16).getOrElse(Locale.ENGLISH))
    }



  }


}