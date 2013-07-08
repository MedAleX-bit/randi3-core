package org.randi3.service

import org.randi3.randomization.RandomizationMethod
import scalaz._
import org.randi3.utility._
import org.randi3.dao._
import org.randi3.model._
import scala.Left
import scala.Right
import scala.Left
import scalaz.Failure
import scala.Right
import scalaz.Success

trait TrialServiceComponent {

  this: TrialDaoComponent with
    TrialSubjectDaoComponent with
    RandomizationMethodDaoComponent with
    TreatmentArmDaoComponent with
    CriterionDaoComponent with
    UtilityMailComponent with
    MailSenderComponent with
    TrialRightDaoComponent with
    UtilityDBComponent with
    SecurityComponent =>

  val trialService: TrialService

  class TrialService {

    import securityUtility._
    import utilityDB._

    def create(trial: Trial, principalInvestigator: User): Validation[String, Int] = {
      checkUserCanCreate(trial, code = {
        trialDao.create _
      }
      ).toEither match {
        case Left(x) => Failure(x)
        case Right(id) => {
          trialDao.get(id).toEither match {
            case Left(x) => return Failure("Not able to grant rights")
            case Right(trialDB) => {
              val trialAdminRight = TrialRight(Role.trialAdministrator, trialDB.get).toOption.get
              trialRightDao.addRight(currentUser.get.id, trialAdminRight).toEither match {
                case Left(x) => return Failure("Not able to grant rights")
                case _ =>
              }

              val trialPInvestigatorRight = TrialRight(Role.principleInvestigator, trialDB.get).toOption.get
              trialRightDao.addRight(principalInvestigator.id, trialPInvestigatorRight).toEither match {
                case Left(x) => return Failure("Not able to grant rights")
                case _ =>
              }

            }
          }
          Success(id)
        }
      }

    }

    def addRandomizationMethod(trial: Trial, randomizationMethod: RandomizationMethod): Validation[String, Trial] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      checkUserCanUpdate(trial, dbTrial, randomizationMethod, code = {
        trialDao.addRandomizationMethod _
      })

    }

    def update(trial: Trial): Validation[String, Trial] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      if(dbTrial.status == TrialStatus.IN_PREPARATION){
      checkUserCanUpdate(trial, dbTrial, code = {
        val removedTreatmentArms = dbTrial.treatmentArms.filter(dbArm => !trial.treatmentArms.map(_.id).contains(dbArm.id))
        removedTreatmentArms.foreach(treatmentArmDao.delete(_))
        val removedCriterions = dbTrial.criterions.filter(dbCriterion => !trial.criterions.map(_.id).contains(dbCriterion.id))
        removedCriterions.foreach(criterion => criterionDao.delete(criterion.id))

        if(dbTrial.randomizationMethod.isDefined){
          if(!trial.randomizationMethod.isDefined)  {
            //randomization method removed
            randomizationMethodDao.delete(dbTrial.randomizationMethod.get)
          }else {
              randomizationMethodDao.delete(dbTrial.randomizationMethod.get)
              randomizationMethodDao.create(trial.randomizationMethod.get, trial.id)
          }
        }else {
          if (trial.randomizationMethod.isDefined)
            randomizationMethodDao.create(trial.randomizationMethod.get, trial.id)
        }

        trialDao.update _
      })
      }else if((dbTrial.status == TrialStatus.ACTIVE && (trial.status == TrialStatus.FINISHED || trial.status == TrialStatus.PAUSED))
       ||
        (dbTrial.status == TrialStatus.PAUSED && (trial.status == TrialStatus.FINISHED || trial.status == TrialStatus.ACTIVE))
      ){
        checkUserCanUpdate(dbTrial.copy(status = trial.status), dbTrial, code = {
          trialDao.updateStatus _
        })

      }  else {
        Failure("Can't change trial")
      }
    }

    def saveParticipatingSites(trial: Trial): Validation[String, Trial] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      if((dbTrial.status == TrialStatus.ACTIVE ||  dbTrial.status == TrialStatus.PAUSED) && dbTrial.isTrialOpen ){
        val newParticipatingSites = trial.participatingSites.map(site => site.id)
        if (!dbTrial.participatingSites.map(site => site.id).map(oldSiteId => newParticipatingSites.contains(oldSiteId)).reduce((acc, act) => acc && act)){
          Failure("Can't remove participating sites")
        } else
        checkUserCanUpdate(dbTrial.copy(participatingSites = trial.participatingSites), dbTrial, code = {
           trialDao.update _
        })
      } else{
        Failure("Can't change trial")
      }
    }

    def getAll: Validation[String, List[Trial]] = {
      filterList(trialDao.getAll)
    }

    def get(id: Int): Validation[String, Option[Trial]] = {
      filterElement(trialDao.get(id))
    }

    def delete(trial: Trial) {
      //TODO security check
      trialDao.delete(trial)
    }

    def randomize(trial: Trial, trialSubject: TrialSubject): Validation[String, (TreatmentArm, String)] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      if (dbTrial.status == TrialStatus.ACTIVE) {
        val identifier =  dbTrial.getSubjects.map(subject => subject.identifier)
        checkUserCanRandomize(dbTrial, trialSubject, code = {
          val subject = trialSubject.copy(investigatorUserName = currentUser.get.username)
          dbTrial.randomize(subject).toEither match {
            case Left(x) => Failure(x) //randomization failure
            case Right(treatmentArm) => {
              val subjectWithIdentification = subject.copy(identifier = TrialSubjectIdentificationCreator.createIdentification(dbTrial, treatmentArm, subject))
              if (!identifier.contains(subjectWithIdentification.identifier)) {
                trialSubjectDao.create(subjectWithIdentification, treatmentArm.id).toEither match {
                  case Left(x) => Failure("Can't create trial subject: " + x)
                  case _ => randomizationMethodDao.update(dbTrial.randomizationMethod.get).toEither match {
                    case Left(x) => Failure("Can't update randomization method: " + x)
                    case _ => {
                      mailSender.sendMessage(currentUser.get.email, utilityMail.getRandomizedMailCCAddresses(dbTrial), "", "[" + dbTrial.abbreviation+ "] " + "Patient randomized" , utilityMail.getRandomizedMailContent(dbTrial, treatmentArm, subjectWithIdentification))
                      Success((treatmentArm, subjectWithIdentification.identifier))
                    }
                  }
                }
              } else {
                Failure("Duplicated subject identifier")
              }
            }
          }
        })
      } else {
        Failure("Trial is not ACTIVE")
      }

    }

  }

}
