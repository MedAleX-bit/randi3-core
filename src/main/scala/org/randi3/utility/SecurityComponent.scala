package org.randi3.utility

import org.randi3.model._
import org.randi3.randomization.RandomizationMethod
import scalaz._
import Scalaz._
import collection.mutable.ListBuffer
import org.randi3.dao.{TrialDaoComponent, UserDaoComponent}
import scala.Left
import scalaz.Failure
import scala.Some
import scala.Right
import scalaz.Success

trait SecurityComponent {

  this: UtilityDBComponent with
  I18NComponent =>

  val securityUtility: SecurityUtility

  abstract class SecurityUtility extends AbstractSecurityUtil{

    import utilityDB._
    import i18n._



    final def filterList[T](resultList: Validation[String, List[T]]): Validation[String, List[T]] = {
      if (currentUser.isDefined) {
        if (resultList.isSuccess) {
          val result = if (resultList.toOption.get.isEmpty) return resultList else resultList.toOption.get
          if (result.head.getClass == classOf[Trial]) {
            Success(result.asInstanceOf[List[Trial]].filter(trial => currentUser.get.rights.map(right => right.trial.id).contains(trial.id)).asInstanceOf[List[T]])

          } else if (result.head.getClass == classOf[TrialSite]) {
            resultList

          } else if (result.head.getClass == classOf[User]) {
            //TODO
            resultList

          } else Failure("Can't check and filter list")
        } else resultList
      } else Failure(text("failure.userNotLoggedIn"))
    }

    final def filterElement[T](result: Validation[String, Option[T]]): Validation[String, Option[T]] = {
      if (currentUser.isDefined) {
        if (result.isSuccess) {

          val res = if (result.toOption.get.isDefined) result.toOption.get.get else return result

          if (res.getClass == classOf[Trial]) {
            val actTrial = res.asInstanceOf[Trial]
            //Check if a right to this trial exists
            if (currentUser.get.rights.map(right => right.trial.id).contains(actTrial.id)) {
              val roleList = currentUser.get.rights.filter(right => right.trial.id == actTrial.id).map(right => right.role)
              //Statistician, Monitor, Principal Investigator and Trial Administrator can access the whole trial
              if (roleList.contains(Role.monitor) || roleList.contains(Role.principleInvestigator) || roleList.contains(Role.statistician) || roleList.contains(Role.trialAdministrator)) {
                result
                //The investigator has only access to his own trial subjects
              } else if (roleList.contains(Role.investigator)) {
                val filteredArms = new ListBuffer[TreatmentArm]()
                actTrial.treatmentArms.foreach(arm => filteredArms += arm.copy(subjects = arm.subjects.filter(subject => subject.investigatorUserName == currentUser.get.username)))
                Success(Some(actTrial.copy(treatmentArms = filteredArms.toList).asInstanceOf[T]))
              } else Failure("No rights for trial: " + actTrial.name)
            } else Failure("No rights for trial: " + actTrial.name)

          } else if (res.getClass == classOf[User]) {
            //Nothing to check
            result
          } else if (res.getClass == classOf[TrialSite]) {
            //Nothing to check
            result
          } else Failure("Can't check object")
        } else result
      } else Failure(text("failure.userNotLoggedIn"))
    }

    final def checkUserCanUpdate[T <: Entity](element: T, dbElement: T, code: T => Validation[String, T]): Validation[String, T] = {
      if (currentUser.isDefined) {
        val updateText = new StringBuilder
        updateText.append("Object changed: ")
        if (element.getClass == classOf[Trial]) {

          val roleList = currentUser.get.rights.filter(right => right.trial.id == element.asInstanceOf[Trial].id)
            .map(right => right.role)
          if (!(roleList.contains(Role.principleInvestigator) ||
            roleList.contains(Role.trialAdministrator))){
            return Failure("can't update")
          }
          val trial = element.asInstanceOf[Trial]
          val dbTrial = dbElement.asInstanceOf[Trial]
          updateText.append(findTrialChanges(dbTrial, trial))

        } else if (element.getClass == classOf[TrialSite]) {
          if (!currentUser.get.administrator) return Failure("No rights to update trial site.")
          val site = element.asInstanceOf[TrialSite]
          val dbSite = dbElement.asInstanceOf[TrialSite]
          updateText.append(findTrialSiteChanges(dbSite, site))

        } else if (element.getClass == classOf[User]) {
          if (!currentUser.get.administrator) {
            val actUser = element.asInstanceOf[User]
            val curUser = currentUser.get
            if (currentUser.get.id == actUser.id) {
              //a user can't change his own rights
              if (!(actUser.administrator == curUser.administrator && actUser.canCreateTrial == curUser.canCreateTrial && actUser.rights == curUser.rights))
                return Failure("A user can't change his own rights")

            } else return Failure("No rights to update the user.")
            val dbUser =dbElement.asInstanceOf[User]
            updateText.append(findUserChanges(dbUser, actUser))
          }

        } else return Failure("Can't check")
        code.apply(element).toEither match {
          case Left(x) => Failure(x)
          case Right(entity) => {
            logAudit(currentUser.get, ActionType.UPDATE, entity, updateText.toString())
            Success(entity)
          }
        }
      } else Failure(text("failure.userNotLoggedIn"))
    }

    private def findTrialChanges(oldTrial: Trial, newTrial: Trial): String = {
      val result = new StringBuilder
      if(oldTrial.name != newTrial.name) result.append("New name = " + newTrial.name + "; ")
      if(oldTrial.abbreviation != newTrial.abbreviation) result.append("New abbreviation = " + newTrial.abbreviation + "; ")
      if(oldTrial.description != newTrial.description) result.append("New description = " + newTrial.description + "; ")
      if(oldTrial.startDate != newTrial.startDate) result.append("New start date = " + newTrial.startDate + "; ")
      if(oldTrial.endDate != newTrial.endDate) result.append("New end date = " + newTrial.endDate + "; ")
      if(oldTrial.isTrialOpen != newTrial.isTrialOpen) result.append("New trial open status = " + newTrial.isTrialOpen.toString + "; ")
      if(oldTrial.isStratifiedByTrialSite != newTrial.isStratifiedByTrialSite) result.append("New trial site stratification status = " + newTrial.isStratifiedByTrialSite.toString + "; ")
      if(oldTrial.status != newTrial.status) result.append("New status= " + newTrial.status.toString + "; ")
      if(oldTrial.identificationCreationType != newTrial.identificationCreationType) result.append("New identification creation type= " + newTrial.identificationCreationType.toString + "; ")
      result.toString()
    }

    private def findUserChanges(oldUser: User, newUser: User): String = {
      val result = new StringBuilder
      if(oldUser.administrator != newUser.administrator) result.append("New administrator status = " + newUser.administrator + "; ")
      if(oldUser.canCreateTrial != newUser.canCreateTrial) result.append("New \"can create trial\" status = " + newUser.canCreateTrial + "; ")
      if(oldUser.email != newUser.email) result.append("New email = " + newUser.email + "; ")
      if(oldUser.firstName != newUser.firstName) result.append("New first name = " + newUser.firstName + "; ")
      if(oldUser.isActive != newUser.isActive) result.append("New is active status = " + newUser.isActive + "; ")
      if(oldUser.lastName != newUser.lastName) result.append("New last name = " + newUser.lastName + "; ")
      if(oldUser.locale != newUser.locale) result.append("New locale = " + newUser.locale.toString + "; ")
      if(oldUser.password != newUser.password) result.append("New password" )
      if(oldUser.phoneNumber != newUser.phoneNumber) result.append("New phone number = " + newUser.phoneNumber + "; ")

      result.toString()
    }


    private def findTrialSiteChanges(oldSite: TrialSite, newSite: TrialSite): String = {
      val result = new StringBuilder
      if(oldSite.city != newSite.city) result.append("New city =" + newSite.city + "; ")
      if(oldSite.country != newSite.country) result.append("New country =" + newSite.country + "; ")
      if(oldSite.isActive != newSite.isActive) result.append("New is active status =" + newSite.isActive + "; ")
      if(oldSite.name != newSite.name) result.append("New name =" + newSite.name + "; ")
      if(oldSite.password != newSite.password) result.append("New password; ")
      if(oldSite.postCode != newSite.postCode) result.append("New post code =" + newSite.postCode + "; ")
      if(oldSite.street != newSite.street) result.append("New street =" + newSite.street + "; ")

      result.toString()
    }

    final def checkUserCanUpdate(trial: Trial, trialDb: Trial, randomizationMethod: RandomizationMethod, code: (Trial, RandomizationMethod) => Validation[String, Trial]): Validation[String, Trial] = {
      if (currentUser.isDefined) {
        def dummyFunction(trial: Trial): Validation[String, Trial] = {
          Success(trial)
        }
        checkUserCanUpdate(trial, trialDb, dummyFunction _).toEither match {
          case Left(x) => Failure(x)
          case Right(_) => code.apply(trial, randomizationMethod)
        }
      } else Failure(text("failure.userNotLoggedIn"))
    }

    private def checkUserCanChangeRight(user: User, trialRight: TrialRight, dbTrial: Trial, auditText: String, code: (Int, TrialRight) => Validation[String, TrialRight]): Validation[String, TrialRight] = {
      if (currentUser.isDefined) {
        val curUser = currentUser.get
        val listTrialRights = curUser.rights.filter(right => right.trial.id == trialRight.trial.id)
        if (listTrialRights.isEmpty) {
          Failure("User has not the necessary rights")
        }else if(listTrialRights.map(right => right.role).filter(role => role == Role.principleInvestigator || role == Role.trialAdministrator).isEmpty) {
          Failure("User has not the necessary rights")
        } else if(!dbTrial.participatingSites.map(site =>site.id).contains(user.site.id)) {
          Failure("User doesn't belong to the participating trial sites")
        } else
        code.apply(user.id, trialRight).toEither match {
          case Right(b) => {
            logAudit(curUser, ActionType.UPDATE, trialRight.trial, auditText)
            Success(b)
          }
          case Left(failure) => Failure(failure)
        }
      } else Failure(text("failure.userNotLoggedIn"))
    }

    final def checkUserCanAddRight(user: User, trialRight: TrialRight,  dbTrial: Trial, code: (Int, TrialRight) => Validation[String, TrialRight]): Validation[String, TrialRight] = {
      checkUserCanChangeRight(user, trialRight, dbTrial, "Added " + trialRight.role.toString +" in trial " + trialRight.trial.abbreviation +" to user " + user.username, code)
    }
    final def checkUserCanRemoveRight(user: User, trialRight: TrialRight, dbTrial: Trial, code: (Int, TrialRight) => Validation[String, TrialRight]): Validation[String, TrialRight] = {
      checkUserCanChangeRight(user, trialRight, dbTrial, "Removed " + trialRight.role.toString +" in trial " + trialRight.trial.abbreviation +" from user " +  user.username, code)
    }



    final def checkUserCanRegister(newUser: User, code: User => Validation[String, Int]): Validation[String, Int] = {
      if (!newUser.administrator || !newUser.canCreateTrial || newUser.rights.isEmpty)
        code.apply(newUser).toEither match {
          case Left(x) => Failure(x)
          case Right(identifier) => {
            logAudit(newUser, ActionType.CREATE, newUser.copy(id = identifier), "audit.userRegisterd")
            Success(identifier)
          }
        }
      else Failure("Can't register user with rights")

    }


    final def checkUserCanCreate[T <: Entity](element: T, code: T => Validation[String, Int]): Validation[String, Int] = {
      if (currentUser.isDefined) {
        if (element.getClass == classOf[Trial]) {
          if (!currentUser.get.canCreateTrial) return Failure("No rights to create a trial")

        } else if (element.getClass == classOf[TrialSite]) {
          if (!currentUser.get.administrator) return Failure("No rights to create a trial site")

        } else if (element.getClass == classOf[User]) {
          if (!currentUser.get.administrator) {
            val roleList = currentUser.get.rights.filter(right => right.trial.id == element.asInstanceOf[Trial].id)
              .map(right => right.role)
            if (!(roleList.contains(Role.principleInvestigator) ||
              roleList.contains(Role.trialAdministrator))) {
              val user = element.asInstanceOf[User]
              if (user.administrator || user.canCreateTrial || !user.rights.isEmpty)
                return Failure("No right to create a user with admin and trial creation rights")
            }
          }

        } else return Failure("Can't check")

        code.apply(element).toEither match {
          case Left(x) => Failure(x)
          case Right(identifier) => {
            logAudit(currentUser.get, ActionType.CREATE, element, identifier, "audit.objectCreated")
            Success(identifier)
          }
        }
      } else Failure(text("failure.userNotLoggedIn"))
    }

    final def checkUserCanRandomize(trial: Trial, trialSubject: TrialSubject, code: => Validation[String, (TreatmentArm, String)]): Validation[String, (TreatmentArm, String)] = {
      if (currentUser.isDefined) {
        if (currentUser.get.rights.filter(right => right.trial.id == trial.id).map(right => right.role).contains(Role.investigator)
        && trial.participatingSites.map(site => site.id).contains(currentUser.get.site.id)) {
          code.toEither match {
            case Left(x) => Failure(x)
            case Right(result) => {
              logAudit(currentUser.get, ActionType.RANDOMIZE, trial, "Randomized subject (" + result._2 + ") to arm " + result._1.name)
              Success(result)
            }
          }
        } else Failure(text("failure.cantRandomize"))
      } else Failure(text("failure.userNotLoggedIn"))
    }

    final def checkUserCanAddStage(trial: Trial, stageName: String, trialSubject: TrialSubject, code: => Validation[String,TrialSubject]): Validation[String, TrialSubject] = {
      if (currentUser.isDefined) {
        if (currentUser.get.rights.filter(right => right.trial.id == trial.id).map(right => right.role).contains(Role.investigator)
          && trial.participatingSites.map(site => site.id).contains(currentUser.get.site.id)
          && trialSubject.trialSite.id == currentUser.get.site.id) {
          code.toEither match {
            case Left(x) => Failure(x)
            case Right(result) => {
              logAudit(currentUser.get, ActionType.ADD_RESPONSE, trial, "Add stage ("+ stageName +") to subject: " + result.identifier)
              Success(result)
            }
          }
        } else Failure(text("failure.cantAddResponse"))
      } else Failure(text("failure.userNotLoggedIn"))
    }

  }

}

abstract class AbstractSecurityUtil {
  def currentUser: Option[User]
}