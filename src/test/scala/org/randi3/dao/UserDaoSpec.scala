package org.randi3.dao

import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.specs.runner.JUnitSuiteRunner
import org.randi3.model._
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitSuiteRunner])
class UserDaoSpec extends Spec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._

  describe("The UserDao create method") {

    it("should be able to create a user with site and without rights") {
      val usersCount = database withSession {
        Query(Users).list.size
      }
      val trialSiteDB = createTrialSiteDB

      val user: User = createUser.copy(site = trialSiteDB)

      val id = userDao.create(user).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      database withSession {
        val allUsers = Query(Users).list
        allUsers.size must be(usersCount + 1)

        val users = allUsers.filter(user => user._1 == id)

        users.size must be(1)
        users.head._3 must be(user.username)
        users.head._4 must be(user.email)
        users.head._5 must be(user.firstName)
        users.head._6 must be(user.lastName)
        users.head._7 must be(user.phoneNumber)
        users.head._8 must be(user.site.id)
        users.head._9 must be(user.password)
      }
    }

    it("should be able to create a user with site and rights")(pending)

  }

  describe("The UserDao get method") {

    it("should be able to get a user with site and without rights (empty list)") {
      val usersCount = database withSession {
        Query(Users).list.size
      }
      val trialSiteDB: TrialSite = createTrialSiteDB

      val user: User = createUser.copy(site = trialSiteDB)

      val id = userDao.create(user).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      id must be > Int.MinValue

      userDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(userDB)) => {
          userDB.id must be(id)
          userDB.username must be(user.username)
          userDB.email must be(user.email)
          userDB.firstName must be(user.firstName)
          userDB.lastName must be(user.lastName)
          userDB.phoneNumber must be(user.phoneNumber)
          userDB.site.id must be(user.site.id)
          userDB.rights must not be (null)
          userDB.rights.size must be(0)
        }
      }

    }

  }

  describe("The UserDao getAll method") {

    it("should return all users") {
      val trialSite1 = createTrialSiteDB
      val trialSite2 = createTrialSiteDB

      val users = new ListBuffer[User]()

      database withSession {
        val allUsers = Query(Users).list
        for (user <- allUsers) {
          userDao.get(user._1).either match {
            case Left(x) => fail(x)
            case Right(None) => fail("user not found")
            case Right(Some(userDB)) => users.append(userDB)
          }
        }
      }

      for (i <- 1 to 10) {
        users += userDao.get(userDao.create(createUser.copy(site = trialSite1)).toOption.get).toOption.get.get
      }
      for (i <- 1 to 10) {
        users += userDao.get(userDao.create(createUser.copy(site = trialSite2)).toOption.get).toOption.get.get
      }

      val usersDB = userDao.getAll.either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      usersDB.size must be(users.size)

      for (userDB <- usersDB) {
        val user = users.filter(u => u.id == userDB.id).head
        userDB.id must be(user.id)
        userDB.username must be(user.username)
        userDB.email must be(user.email)
        userDB.firstName must be(user.firstName)
        userDB.lastName must be(user.lastName)
        userDB.phoneNumber must be(user.phoneNumber)
        userDB.site.id must be(user.site.id)
        userDB.rights must not be (null)
      }

    }
  }

  describe("The UserDao getUsersFromTrialSite method") {

    it("should be able to return the user from one trial site") {
      val trialSite1 = createTrialSiteDB
      val trialSite2 = createTrialSiteDB


      val usersTrialSite1 = for {i <- 1 to 10
      } yield userDao.get(userDao.create(createUser.copy(site = trialSite1)).toOption.get).toOption.get.get

      val usersTrialSite2 = for {i <- 1 to 10
      } yield userDao.get(userDao.create(createUser.copy(site = trialSite2)).toOption.get).toOption.get.get

      val usersTrialSite1DB = userDao.getUsersFromTrialSite(trialSite1.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      usersTrialSite1DB.size must be(usersTrialSite1.size)

      for (userDB <- usersTrialSite1DB) {
        val user = usersTrialSite1.filter(u => u.id == userDB.id).head
        userDB.id must be(user.id)
        userDB.username must be(user.username)
        userDB.email must be(user.email)
        userDB.firstName must be(user.firstName)
        userDB.lastName must be(user.lastName)
        userDB.phoneNumber must be(user.phoneNumber)
        userDB.site.id must be(user.site.id)
        userDB.rights must not be (null)
        userDB.rights.size must be(0)
      }

      val usersTrialSite2DB = userDao.getUsersFromTrialSite(trialSite2.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      usersTrialSite2DB.size must be(usersTrialSite2.size)

      for (userDB <- usersTrialSite2DB) {
        val user = usersTrialSite2.filter(u => u.id == userDB.id).head
        userDB.id must be(user.id)
        userDB.username must be(user.username)
        userDB.email must be(user.email)
        userDB.firstName must be(user.firstName)
        userDB.lastName must be(user.lastName)
        userDB.phoneNumber must be(user.phoneNumber)
        userDB.site.id must be(user.site.id)
        userDB.rights must not be (null)
        userDB.rights.size must be(0)
      }

    }

  }

  describe("The UserDao update methode") {

    it("should not be able to update the username") {
      val userDB = createUserDB

      val changedUser = userDB.copy(username = userName)

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.username must not be (changedUser.username)
          user.password must be(userDB.password)
          user.email must be(userDB.email)
          user.firstName must be(userDB.firstName)
          user.lastName must be(userDB.lastName)
          user.phoneNumber must be(userDB.phoneNumber)
          user.site.id must be(userDB.site.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

    it("should be able to update the password") {
      val userDB = createUserDB

      val changedUser = userDB.copy(password = "password2")

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.password must be(User.hashPassword(changedUser, changedUser.password).toOption.get)
          user.email must be(userDB.email)
          user.firstName must be(userDB.firstName)
          user.lastName must be(userDB.lastName)
          user.phoneNumber must be(userDB.phoneNumber)
          user.site.id must be(userDB.site.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

    it("should be able to update the email adress") {
      val userDB = createUserDB

      val changedUser = userDB.copy(email = "def@abc.de")

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.password must be(userDB.password)
          user.email must be(changedUser.email)
          user.firstName must be(userDB.firstName)
          user.lastName must be(userDB.lastName)
          user.phoneNumber must be(userDB.phoneNumber)
          user.site.id must be(userDB.site.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

    it("should be able to update the first name") {
      val userDB = createUserDB

      val changedUser = userDB.copy(firstName = "firstName2")

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.password must be(userDB.password)
          user.email must be(userDB.email)
          user.firstName must be(changedUser.firstName)
          user.lastName must be(userDB.lastName)
          user.phoneNumber must be(userDB.phoneNumber)
          user.site.id must be(userDB.site.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

    it("should be able to update the last name") {
      val userDB = createUserDB

      val changedUser = userDB.copy(lastName = "lastName2")

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.password must be(userDB.password)
          user.email must be(userDB.email)
          user.firstName must be(userDB.firstName)
          user.lastName must be(changedUser.lastName)
          user.phoneNumber must be(userDB.phoneNumber)
          user.site.id must be(userDB.site.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

    it("should be able to update the phone number") {
      val userDB = createUserDB

      val changedUser = userDB.copy(phoneNumber = "654321")

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.password must be(userDB.password)
          user.email must be(userDB.email)
          user.firstName must be(userDB.firstName)
          user.lastName must be(userDB.lastName)
          user.phoneNumber must be(changedUser.phoneNumber)
          user.site.id must be(userDB.site.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

    it("should be able to update the trial site") {
      val userDB = createUserDB

      val newTrialSiteDB: TrialSite = createTrialSiteDB

      val changedUser = userDB.copy(site = newTrialSiteDB)

      userDao.update(changedUser)

      userDao.get(userDB.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("user not found")
        case Right(Some(user)) => {
          user.username must be(userDB.username)
          user.password must be(userDB.password)
          user.email must be(userDB.email)
          user.firstName must be(userDB.firstName)
          user.lastName must be(userDB.lastName)
          user.phoneNumber must be(userDB.phoneNumber)
          user.site.id must be(newTrialSiteDB.id)
          user.rights must not be (null)
          user.rights.size must be(0)
        }
      }
    }

  }

}