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
      //TODO security
      trialSiteDao.getAll
    }

    def update(trialSite: TrialSite): Validation[String, TrialSite] = {
      val trialSiteDb = trialSiteDao.get(trialSite.id).toOption.get.get
      checkUserCanUpdate(trialSite, trialSiteDb, code = {
        trialSiteDao.update _
      })
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
