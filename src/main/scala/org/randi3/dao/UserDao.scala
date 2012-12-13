package org.randi3.dao

import org.randi3.model._
import org.scalaquery.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import scalaz._
import org.scalaquery.ql.Parameters

import org.randi3.utility._
import scala.Predef._
import org.scalaquery.ql.Query
import scalaz.Digit._9

trait UserDaoComponent {

  this: DaoComponent with
    TrialSiteDaoComponent with
    TrialRightDaoComponent with
    UtilityDBComponent =>

  val userDao: UserDao

  class UserDao {

    import driver.Implicit._
    import schema._
    import utilityDB._

    private val queryUserFromId = for {
      id <- Parameters[Int]
      user <- Users if user.id is id
    } yield user.id ~ user.version ~ user.username ~ user.email ~ user.firstName ~ user.lastName ~ user.phoneNumber ~ user.siteId ~ user.password ~ user.administrator ~ user.canCreateTrials ~ user.isActive

    private val queryUserFromUsername = for {
      username <- Parameters[String]
      user <- Users if user.username is username
    } yield user.id

    private val queryUserFromTrialSite = for {
      trialSiteId <- Parameters[Int]
      user <- Users if user.siteId is trialSiteId
    } yield user.id ~ user.version ~ user.username ~ user.email ~ user.firstName ~ user.lastName ~ user.phoneNumber ~ user.siteId ~ user.password ~ user.administrator ~ user.canCreateTrials ~ user.isActive

    private def queryUserFromTrial(trialId: Int) = for {
      trialRight <- Rights if trialRight.trialId is trialId
      user <- Users if user.id is trialRight.userId
      _ <- Query groupBy user.id
    } yield user.id ~ user.version ~ user.username ~ user.email ~ user.firstName ~ user.lastName ~ user.phoneNumber ~ user.siteId ~ user.password ~ user.administrator ~ user.canCreateTrials  ~ user.isActive


    def create(user: User): Validation[String, Int] = {
      onDB {
        threadLocalSession withTransaction {
          Users.noId insert(user.version, user.username, user.email, user.firstName, user.lastName, user.phoneNumber, user.site.id, user.password, user.administrator, user.canCreateTrial, user.isActive)
        }
        getId(user.username).either match {
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
          val userRow = result(0)
          val trialSite = trialSiteDao.get(userRow._8).either match {
            case Left(x) => return Failure(x)
            case Right(None) => return Failure("trial site not found")
            case Right(Some(ts)) => ts
          }
          trialRightDao.getAll(userRow._1).either match {
            case Left(x) => Failure(x)
            case Right(trialRights) =>
              User(id = userRow._1, version = userRow._2, username = userRow._3, email = userRow._4, firstName = userRow._5, lastName = userRow._6, phoneNumber = userRow._7, site = trialSite, password = userRow._9, rights = trialRights, administrator = userRow._10, canCreateTrial = userRow._11, isActive = userRow._12).either match {
                case Left(x) => Failure("Database entry corrupt: " + x.toString)
                case Right(user) => Success(Some(user))
              }
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
        val passwordHash = User.hashPassword(user, user.password).either match {
          case Left(x) => return Failure(x.toString())
          case Right(password) => password
        }
        threadLocalSession withTransaction {
          queryUserFromId(user.id).mutate {
            r =>
              r.row = r.row.copy(_2 = user.version, _4 = user.email, _5 = user.firstName, _6 = user.lastName, _7 = user.phoneNumber, _8 = user.site.id, _9 = passwordHash, _10 = user.administrator, _11 = user.canCreateTrial, _12 = user.isActive)
          }
        }

        trialRightDao.updateRights(user.id, user.rights).either match {
          case Left(x) => return Failure(x)
          case Right(_) =>
        }


        get(user.id).either match {
          case Left(x) => Failure(x)
          case Right(None) => Failure("user not found")
          case Right(Some(userUpdated)) => Success(userUpdated)
        }
      }
    }

    def getAll: Validation[String, List[User]] = {
      onDB {
        val results = new ListBuffer[User]()
        val trialSites = trialSiteDao.getAll.either match {
          case Left(x) => return Failure(x)
          case Right(ts) => ts
        }
        for (userRow <- Query(Users).list) {
          val trialSite = trialSites.find(site => site.id == userRow._8) match {
            case None => return Failure("trial sites not found")
            case Some(ts) => ts
          }
          trialRightDao.getAll(userRow._1).either match {
            case Left(x) => return Failure(x)
            case Right(trialRights) =>
              User(id = userRow._1, version = userRow._2, username = userRow._3, email = userRow._4, firstName = userRow._5, lastName = userRow._6, phoneNumber = userRow._7, site = trialSite, password = userRow._9, rights = trialRights, administrator = userRow._10, canCreateTrial = userRow._11, isActive = userRow._12).either match {
                case Left(x) => return Failure("Database entry corrupt: " + x.toString)
                case Right(user) => results += user
              }
          }

        }
        Success(results.toList)
      }
    }

    /**
     *
     * @return Failure or an list with the system administrators (only rudimentary infos)
     */
    def getAllAdministrators: Validation[String, List[User]] = {
      //TODO refactor duplicated code
      onDB {
        val results = new ListBuffer[User]()
        val trialSites = trialSiteDao.getAll.either match {
          case Left(x) => return Failure(x)
          case Right(ts) => ts
        }
        for (userRow <- Query(Users).list) {
          if(userRow._10 && userRow._12) {
          val trialSite = trialSites.find(site => site.id == userRow._8) match {
            case None => return Failure("trial sites not found")
            case Some(ts) => ts
          }
          User(username = userRow._3, email = userRow._4, firstName = userRow._5, lastName = userRow._6, phoneNumber = userRow._7, site = trialSite, password = userRow._9, rights = Set()).either match {
            case Left(x) => return Failure("Database entry corrupt: " + x.toString)
            case Right(user) => results += user
          }
          }
        }
        Success(results.toList)
      }
    }


    def getUsersFromTrialSite(trialSiteId: Int): Validation[String, List[User]] = {
      //TODO refactor duplicated code
      onDB {
        val results = new ListBuffer[User]()
        val trialSite = trialSiteDao.get(trialSiteId).either match {
          case Left(x) => return Failure(x)
          case Right(None) => return Failure("trial site not found")
          case Right(Some(ts)) => ts
        }
        for (userRow <- queryUserFromTrialSite(trialSiteId)) {
          trialRightDao.getAll(userRow._1).either match {
            case Left(x) => return Failure(x)
            case Right(trialRights) => User(id = userRow._1, version = userRow._2, username = userRow._3, email = userRow._4, firstName = userRow._5, lastName = userRow._6, phoneNumber = userRow._7, site = trialSite, password = userRow._9, rights = trialRights, administrator = userRow._10, canCreateTrial = userRow._11, isActive = userRow._12).either match {
              case Left(x) => return Failure("Database entry corrupt: " + x.toString)
              case Right(user) => results += user
            }
          }
        }
        Success(results.toList)
      }
    }


    def getUsersFromTrial(trialId: Int): Validation[String, List[User]] = {
      //TODO refactor duplicated code
      onDB {
        val results = new ListBuffer[User]()
        val trialSites = trialSiteDao.getAll.either match {
          case Left(x) => return Failure(x)
          case Right(ts) => ts
        }
        for (userRow <- queryUserFromTrial(trialId)) {
          val trialSite = trialSites.find(site => site.id == userRow._8) match {
            case None => return Failure("trial sites not found")
            case Some(ts) => ts
          }
          trialRightDao.getAll(userRow._1).either match {
            case Left(x) => return Failure(x)
            case Right(trialRights) => User(id = userRow._1, version = userRow._2, username = userRow._3, email = userRow._4, firstName = userRow._5, lastName = userRow._6, phoneNumber = userRow._7, site = trialSite, password = userRow._9, rights = trialRights, administrator = userRow._10, canCreateTrial = userRow._11, isActive = userRow._12).either match {
              case Left(x) => return Failure("Database entry corrupt: " + x.toString)
              case Right(user) => results += user
            }
          }
        }
        Success(results.toList)
      }
    }

    def deactivateUsersFromTrialSite(trialSiteId: Int): Validation[String, String] = {
      //TODO refactor duplicated code
      onDB {
        val results = new ListBuffer[User]()
        val trialSite = trialSiteDao.get(trialSiteId).either match {
          case Left(x) => return Failure(x)
          case Right(None) => return Failure("trial site not found")
          case Right(Some(ts)) => ts
        }
        threadLocalSession withTransaction {
        queryUserFromTrialSite(trialSiteId).mutate {
          r =>
            r.row = r.row.copy(_12 = false)
        }
        }
        Success("")
      }
    }




  }

}