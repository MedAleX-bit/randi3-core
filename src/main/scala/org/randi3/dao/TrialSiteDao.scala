package org.randi3.dao

import org.randi3.model._
import org.scalaquery.session.Database.threadLocalSession
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.utility.{I18NComponent, UtilityDBComponent}
import scalaz.Digit.{_9, _8}
import org.scalaquery.ql.Parameters
import org.scalaquery.ql.Query

trait TrialSiteDaoComponent {

  this: DaoComponent with
    UtilityDBComponent with
    I18NComponent =>

  val trialSiteDao: TrialSiteDao

  class TrialSiteDao {

    import driver.Implicit._
    import schema._
    import i18n._
    import utilityDB._

    private val queryTrialSiteFromId = for {
      id <- Parameters[Int]
      ts <- TrialSites if ts.id is id
    } yield ts.id ~ ts.version ~ ts.name ~ ts.country ~ ts.postCode ~ ts.city ~ ts.street ~ ts.password  ~ ts.isActive

    private val queryTrialSiteFromName = for {
      name <- Parameters[String]
      ts <- TrialSites if ts.name is name
    } yield ts.id ~ ts.version ~ ts.name ~ ts.country ~ ts.postCode ~ ts.city ~ ts.street ~ ts.password ~ ts.isActive

    private val queryParticipatingSitesFromTrial = for {
      trialId <- Parameters[Int]
      partSites <- ParticipatingSites if partSites.trialId is trialId
      sites <- TrialSites if sites.id is partSites.trialSiteId
    } yield sites

    def create(trialSite: TrialSite): Validation[String, Int] = {
      onDB {
        threadLocalSession withTransaction {
          TrialSites.noId insert(trialSite.version, trialSite.name, trialSite.country, trialSite.postCode, trialSite.city, trialSite.street, trialSite.password, trialSite.isActive)
        }
        getId(trialSite.name)
      }
    }

    private def getId(trialSiteName: String): Validation[String, Int] = {
      for (ts <- queryTrialSiteFromName(trialSiteName)) return Success(ts._1)
      Failure("TrialSite not found")
    }

    def get(id: Int): Validation[String, Option[TrialSite]] = {
      onDB {
        val resultList = queryTrialSiteFromId(id).list
        if (resultList.isEmpty) Success(None)
        else if (resultList.size == 1) {
          val ts = resultList(0)
          TrialSite(id = ts._1, version = ts._2, name = ts._3, country = ts._4, street = ts._7, postCode = ts._5, city = ts._6, password = ts._8, isActive = ts._9).either match {
            case Left(x) => Failure(text("database.entryCorrupt") +" "+ x.toString())
            case Right(trialSite) => Success(Some(trialSite))
          }
        } else Failure("more than one trial site with id=" + id + " found")
      }
    }

    def update(trialSite: TrialSite): Validation[String, TrialSite] = {
      onDB {
        queryTrialSiteFromId(trialSite.id).mutate {
          r =>
            r.row = r.row.copy(_2 = trialSite.version, _3 = trialSite.name, _4 = trialSite.country, _5 = trialSite.postCode, _6 = trialSite.city, _7 = trialSite.street, _8 = trialSite.password, _9 = trialSite.isActive)
        }
        get(trialSite.id).either match {
          case Right(Some(site)) => Success(site)
          case _ => Failure("TrialSite not found")
        }
      }
    }

    def getAll: Validation[String, List[TrialSite]] = {
      onDB {
        generateTrialSiteList(Query(TrialSites).list)
      }
    }

    def getAllActive: Validation[String, List[TrialSite]] = {
      onDB {
        generateTrialSiteList(Query(TrialSites).filter(site => site.isActive).list)
      }
    }


    def getParticipationSites(trialId: Int): Validation[String, List[TrialSite]] = {
      onDB {
        generateTrialSiteList(queryParticipatingSitesFromTrial(trialId).list)
      }
    }

    private def generateTrialSiteList(dbRows: List[(Int, Int, String, String, String, String, String, String, Boolean)]): Validation[String, List[TrialSite]] = {
      val results = new ListBuffer[TrialSite]()
      dbRows.foreach {
        ts =>
          TrialSite(id = ts._1, version = ts._2, name = ts._3, country = ts._4, street = ts._7, postCode = ts._5, city = ts._6, password = ts._8, isActive = ts._9).either match {
            case Left(x) => return Failure(text("database.entryCorrupt") +" "+ x.toString())
            case Right(trialSite) => results += trialSite
          }
      }
      Success(results.toList)
    }

  }

}