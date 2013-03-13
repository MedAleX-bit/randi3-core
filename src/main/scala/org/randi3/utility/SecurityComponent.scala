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

  this: UtilityDBComponent =>

  val securityUtility: SecurityUtility

  abstract class SecurityUtility {

    import utilityDB._

    def currentUser: Option[User]

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
      } else Failure("Not logged in")
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
      } else Failure("Not logged in")
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

        } else if (element.getClass == classOf[User]) {
          if (!currentUser.get.administrator) {
            val actUser = element.asInstanceOf[User]
            val curUser = currentUser.get
            if (currentUser.get.id == actUser.id) {
              //a user can't change his own rights
              if (!(actUser.administrator == curUser.administrator && actUser.canCreateTrial == curUser.canCreateTrial && actUser.rights == curUser.rights))
                return Failure("A user can't change his own rights")

            } else return Failure("No rights to update the user.")
          }

        } else return Failure("Can't check")
        code.apply(element).either match {
          case Left(x) => Failure(x)
          case Right(entity) => {
            logAudit(currentUser.get, ActionType.UPDATE, entity, updateText.toString())
            Success(entity)
          }
        }
      } else Failure("Not logged in")
    }

    private def findTrialChanges(oldTrial: Trial, newTrial: Trial): String = {
      val result = new StringBuilder
      if(oldTrial.name != newTrial.name) result.append("New name = " + newTrial.name + "; ")
      if(oldTrial.abbreviation != newTrial.abbreviation) result.append("New abbreviation = " + newTrial.abbreviation + "; ")
      if(oldTrial.description != newTrial.description) result.append("New description = " + newTrial.description + "; ")
      if(oldTrial.startDate != newTrial.startDate) result.append("New start date = " + newTrial.startDate + "; ")
      if(oldTrial.endDate != newTrial.endDate) result.append("New end date = " + newTrial.endDate + "; ")
      if(oldTrial.stratifyTrialSite != newTrial.stratifyTrialSite) result.append("New stratify trial site = " + newTrial.stratifyTrialSite.toString + "; ")
      if(oldTrial.status != newTrial.status) result.append("New status= " + newTrial.status.toString + "; ")
      if(oldTrial.identificationCreationType != newTrial.identificationCreationType) result.append("New identification creation type= " + newTrial.identificationCreationType.toString + "; ")
      result.toString()
    }

    final def checkUserCanUpdate(trial: Trial, trialDb: Trial, randomizationMethod: RandomizationMethod, code: (Trial, RandomizationMethod) => Validation[String, Trial]): Validation[String, Trial] = {
      if (currentUser.isDefined) {
        def dummyFunction(trial: Trial): Validation[String, Trial] = {
          Success(trial)
        }
        checkUserCanUpdate(trial, trialDb, dummyFunction _).either match {
          case Left(x) => Failure(x)
          case Right(_) => code.apply(trial, randomizationMethod)
        }
      } else Failure("Not logged in")
    }

    private def checkUserCanChangeRight(userId: Int, trialRight: TrialRight, dbTrial: Trial, auditText: String, code: (Int, TrialRight) => Validation[String, TrialRight]): Validation[String, TrialRight] = {
      if (currentUser.isDefined) {
        val curUser = currentUser.get
        val listTrialRights = curUser.rights.filter(right => right.trial.id == trialRight.trial.id)
        if (listTrialRights.isEmpty) {
          Failure("User has not the necessary rights")
        }else if(listTrialRights.map(right => right.role).filter(role => role == Role.principleInvestigator || role == Role.trialAdministrator).isEmpty) {
          Failure("User has not the necessary rights")
        } else
        code.apply(userId, trialRight).either match {
          case Right(b) => {
            logAudit(curUser, ActionType.UPDATE, trialRight.trial, auditText)
            Success(b)
          }
          case Left(failure) => Failure(failure)
        }
      } else Failure("Not logged in")
    }

    final def checkUserCanAddRight(userId: Int, trialRight: TrialRight,  dbTrial: Trial, code: (Int, TrialRight) => Validation[String, TrialRight]): Validation[String, TrialRight] = {
      checkUserCanChangeRight(userId, trialRight, dbTrial, "Added " + trialRight.role.toString +" in trial " + trialRight.trial.abbreviation +" to user " + userId, code)
    }
    final def checkUserCanRemoveRight(userId: Int, trialRight: TrialRight, dbTrial: Trial, code: (Int, TrialRight) => Validation[String, TrialRight]): Validation[String, TrialRight] = {
      checkUserCanChangeRight(userId, trialRight, dbTrial, "Removed " + trialRight.role.toString +" in trial " + trialRight.trial.abbreviation +" from user " + userId, code)
    }



    final def checkUserCanRegister(newUser: User, code: User => Validation[String, Int]): Validation[String, Int] = {
      if (!newUser.administrator || !newUser.canCreateTrial || newUser.rights.isEmpty)
        code.apply(newUser).either match {
          case Left(x) => Failure(x)
          case Right(identifier) => {
            logAudit(newUser, ActionType.CREATE, newUser.copy(id = identifier), "User registered")
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

        code.apply(element).either match {
          case Left(x) => Failure(x)
          case Right(identifier) => {
            logAudit(currentUser.get, ActionType.CREATE, element, identifier, "Object created")
            Success(identifier)
          }
        }
      } else Failure("Not logged in")
    }

    final def checkUserCanRandomize(trial: Trial, trialSubject: TrialSubject, code: => Validation[String, (TreatmentArm, String)]): Validation[String, (TreatmentArm, String)] = {
      if (currentUser.isDefined) {
        if (currentUser.get.rights.filter(right => right.trial.id == trial.id).map(right => right.role).contains(Role.investigator)) {
          code.either match {
            case Left(x) => Failure(x)
            case Right(result) => {
              logAudit(currentUser.get, ActionType.RANDOMIZE, trial, "Randomized subject (" + result._2 + ") to arm " + result._1.name)
              Success(result)
            }
          }
        } else Failure("Can't randomize in this trial")
      } else Failure("Not logged in")
    }

  }

}
