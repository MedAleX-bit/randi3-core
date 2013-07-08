package org.randi3.service

import org.randi3.model.{TrialSite, User}
import scalaz._
import org.randi3.utility.SecurityComponent
import grizzled.string.template.StringTemplate
import org.randi3.dao.{UserDaoComponent, TrialSiteDaoComponent}

trait TrialSiteServiceComponent {

  this: TrialSiteDaoComponent with
    UserDaoComponent with
    SecurityComponent =>

  val trialSiteService: TrialSiteService

  class TrialSiteService {

    import securityUtility._

    def create(trialSite: TrialSite): Validation[String, Int] = {
      checkUserCanCreate(trialSite, code = {
        trialSiteDao.create _
      })
    }

    def getAll: Validation[String, List[TrialSite]] = {
      trialSiteDao.getAll
    }

    def getAllActive: Validation[String, List[TrialSite]] = {
      trialSiteDao.getAllActive
    }

    def update(trialSite: TrialSite): Validation[String, TrialSite] = {
      val trialSiteDb = trialSiteDao.get(trialSite.id).toOption.get.get
      checkUserCanUpdate(trialSite, trialSiteDb, code = {
        trialSiteDao.update _
      })
    }

    def activate(trialSite: TrialSite): Validation[String, TrialSite] = {
      val trialSiteDb = trialSiteDao.get(trialSite.id).toOption.get.get
      val activeSite = trialSiteDb.copy(isActive = true)
      checkUserCanUpdate(activeSite, trialSiteDb, code = {
        trialSiteDao.update _
      })
    }

    def deactivate(trialSite: TrialSite): Validation[String, TrialSite] = {
      val trialSiteDb = trialSiteDao.get(trialSite.id).toOption.get.get
      val activeSite = trialSiteDb.copy(isActive = false)
      val changedSite = checkUserCanUpdate(activeSite, trialSiteDb, code = {
        trialSiteDao.update _
      })
      changedSite.toEither match {
        case Left(failure) => Failure(failure)
        case Right(site) => {
           userDao.deactivateUsersFromTrialSite(site.id).toEither match {
             case Left(failure) => Failure("Site deactivated, but problem with deactivation of the users: " + failure)
             case Right(success) => Success(site)
           }
        }
      }
    }

    def delete(trialSite: TrialSite) {

    }

    def get(id: Int): Validation[String, Option[TrialSite]] = {
      filterElement(trialSiteDao.get(id))
    }

    def getMembers(id: Int): Validation[String, List[User]] = {
      filterList(userDao.getUsersFromTrialSite(id))
    }

  }

}
