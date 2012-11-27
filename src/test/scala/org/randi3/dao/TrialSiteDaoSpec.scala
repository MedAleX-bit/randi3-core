package org.randi3.dao

import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.extended.H2Driver.Implicit._

import org.scalaquery.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model.TrialSite
import org.scalaquery.ql.Query

@RunWith(classOf[JUnitRunner])
class TrialSiteDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._
  import schema._

  describe("The TrialSiteDao create method") {

    it("should be able to create a trial site with all fields") {
      val trialSitesCount = database withSession {
        Query(TrialSites).list.size
      }
      val trialSite: TrialSite = createTrialSite
      val id = trialSiteDao.create(trialSite).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      database withSession {
        val allTrialSites = Query(TrialSites).list
        allTrialSites.size must be(trialSitesCount + 1)

        val trialSites = allTrialSites.filter(t => t._1 == id)

        trialSites.size must be(1)
        trialSites.head._3 must be(trialSite.name)
        trialSites.head._4 must be(trialSite.country)
        trialSites.head._5 must be(trialSite.postCode)
        trialSites.head._6 must be(trialSite.city)
        trialSites.head._7 must be(trialSite.street)
      }
    }

  }

  describe("The TrialSiteDao get method") {

    it("should be able to get a trial site") {
      val trialSite: TrialSite = createTrialSiteDB

      val trialSiteDB = trialSiteDao.get(trialSite.id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial site not found")
        case Right(Some(site)) => site
      }

      trialSiteDB.id must be(trialSite.id)
      trialSiteDB.name must be(trialSite.name)
      trialSiteDB.country must be(trialSite.country)
      trialSiteDB.postCode must be(trialSite.postCode)
      trialSiteDB.city must be(trialSite.city)
      trialSiteDB.street must be(trialSite.street)

    }

  }

  describe("The TrialSiteDao getAll method") {

    it("should be return all trial sites") {
      val trialSitesCount = database withSession {
        Query(TrialSites).list.size
      }
      val newTrialSites = for {i <- 1 to 10} yield createTrialSiteDB

      val trialSitesDB = trialSiteDao.getAll.either match {
        case Left(x) => fail(x)
        case Right(sites) => sites
      }

      trialSitesDB.size must be >= (trialSitesCount + 10)

      for (trialSite <- newTrialSites) {
        val trialSiteDB = trialSitesDB.filter(ts => ts.id == trialSite.id).head
        trialSiteDB.name must be(trialSite.name)
        trialSiteDB.country must be(trialSite.country)
        trialSiteDB.postCode must be(trialSite.postCode)
        trialSiteDB.city must be(trialSite.city)
        trialSiteDB.street must be(trialSite.street)
      }

    }
  }

  describe("The TrialSiteDao update method") {

    it("should be able to update the name") {
      val trialSiteDB = createTrialSiteDB
      val changedTrialSite = trialSiteDB.copy(name = trialSiteName)
      trialSiteDao.update(changedTrialSite)
      trialSiteDao.get(trialSiteDB.id).toOption.get.get.name must be(changedTrialSite.name)
    }

    it("should be able to update the country") {
      val trialSiteDB = createTrialSiteDB
      val changedTrialSite = trialSiteDB.copy(country = "Country2")
      trialSiteDao.update(changedTrialSite)
      trialSiteDao.get(trialSiteDB.id).toOption.get.get.country must be(changedTrialSite.country)
    }

    it("should be able to update the post code") {
      val trialSiteDB = createTrialSiteDB
      val changedTrialSite = trialSiteDB.copy(postCode = "54321")
      trialSiteDao.update(changedTrialSite)
      trialSiteDao.get(trialSiteDB.id).toOption.get.get.postCode must be(changedTrialSite.postCode)
    }

    it("should be able to update the city") {
      val trialSiteDB = createTrialSiteDB
      val changedTrialSite = trialSiteDB.copy(city = "City2")
      trialSiteDao.update(changedTrialSite)
      trialSiteDao.get(trialSiteDB.id).toOption.get.get.city must be(changedTrialSite.city)
    }

    it("should be able to update the street") {
      val trialSiteDB = createTrialSiteDB
      val changedTrialSite = trialSiteDB.copy(street = "Street2")
      trialSiteDao.update(changedTrialSite)
      trialSiteDao.get(trialSiteDB.id).toOption.get.get.street must be(changedTrialSite.street)
    }

  }

}