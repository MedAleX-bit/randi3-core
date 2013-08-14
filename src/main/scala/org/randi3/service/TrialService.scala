package org.randi3.service

import org.randi3.randomization.{RandomizationPluginManagerComponent, RandomizationMethod}
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
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint

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
    SecurityComponent with
    RandomizationPluginManagerComponent =>

  val trialService: TrialService

  class TrialService {

    import securityUtility._
    import utilityDB._

    def create(trial: Trial, principalInvestigator: User): Validation[String, Int] = {

      val trialWithStages = if (trial.randomizationMethod.isDefined) {
        val plugin = randomizationPluginManager.getPluginForMethod(trial.randomizationMethod.get)
        if (plugin.isDefined) {
          trial.copy(stages = plugin.get.randomizationConfigurationOptions()._2)
        } else {
          return Failure("Randomization Plug-in is not defined.")
        }
      } else {
        trial
      }

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

    @deprecated("Covered from update method", "")
    def addRandomizationMethod(trial: Trial, randomizationMethod: RandomizationMethod): Validation[String, Trial] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      checkUserCanUpdate(trial, dbTrial, randomizationMethod, code = {
        trialDao.addRandomizationMethod _
      })

    }

    def update(trial: Trial): Validation[String, Trial] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      if (dbTrial.status == TrialStatus.IN_PREPARATION) {
        val trialStages = trial.copy(stages = getStages(trial))
        checkUserCanUpdate(trialStages, dbTrial, code = {
          val removedTreatmentArms = dbTrial.treatmentArms.filter(dbArm => !trial.treatmentArms.map(_.id).contains(dbArm.id))
          removedTreatmentArms.foreach(treatmentArmDao.delete(_))
          val removedCriterions = dbTrial.criterions.filter(dbCriterion => !trial.criterions.map(_.id).contains(dbCriterion.id))
          removedCriterions.foreach(criterion => criterionDao.delete(criterion.id))

          if (dbTrial.randomizationMethod.isDefined) {
            if (!trial.randomizationMethod.isDefined) {
              //randomization method removed
              randomizationMethodDao.delete(dbTrial.randomizationMethod.get)
            } else {
              randomizationMethodDao.delete(dbTrial.randomizationMethod.get)
              randomizationMethodDao.create(trial.randomizationMethod.get, trial.id)
            }
          } else {
            if (trial.randomizationMethod.isDefined)
              randomizationMethodDao.create(trial.randomizationMethod.get, trial.id)
          }

          trialDao.update _
        })
      } else if ((dbTrial.status == TrialStatus.ACTIVE && (trial.status == TrialStatus.FINISHED || trial.status == TrialStatus.PAUSED))
        ||
        (dbTrial.status == TrialStatus.PAUSED && (trial.status == TrialStatus.FINISHED || trial.status == TrialStatus.ACTIVE))
      ) {
        checkUserCanUpdate(dbTrial.copy(status = trial.status), dbTrial, code = {
          trialDao.updateStatus _
        })

      } else {
        Failure("Can't update trial")
      }
    }

    def saveParticipatingSites(trial: Trial): Validation[String, Trial] = {
      val dbTrial = trialDao.get(trial.id).toOption.get.get
      if ((dbTrial.status == TrialStatus.ACTIVE || dbTrial.status == TrialStatus.PAUSED) && dbTrial.isTrialOpen) {
        val newParticipatingSites = trial.participatingSites.map(site => site.id)
        if (!dbTrial.participatingSites.map(site => site.id).map(oldSiteId => newParticipatingSites.contains(oldSiteId)).reduce((acc, act) => acc && act)) {
          Failure("Can't remove participating sites")
        } else
          checkUserCanUpdate(dbTrial.copy(participatingSites = trial.participatingSites), dbTrial, code = {
            trialDao.update _
          })
      } else {
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
        val identifier = dbTrial.getSubjects.map(subject => subject.identifier)
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
                      mailSender.sendMessage(currentUser.get.email, utilityMail.getRandomizedMailCCAddresses(dbTrial), "", "[" + dbTrial.abbreviation + "] " + "Patient randomized", utilityMail.getRandomizedMailContent(dbTrial, treatmentArm, subjectWithIdentification))
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


    def addStage(trial: Trial, trialSubject: TrialSubject, stageName: String, properties: List[SubjectProperty[_ <: Any]]): Validation[String, TrialSubject] = {
      val dbSubject = trialSubjectDao.get(trialSubject.id).toEither match {
        case Left(failure) => return Failure(failure)
        case Right(trialSubjectOption) => trialSubjectOption.getOrElse(return Failure("Trial subject doesn't exists in database"))
      }
      val dbTrial = trialDao.get(trial.id).toEither match {
        case Left(failure) => return Failure(failure)
        case Right(trialOption) => trialOption.getOrElse(return Failure("Trial doesn't exists in database"))
      }

      if (dbTrial.status == TrialStatus.ACTIVE) {
        checkUserCanAddStage(dbTrial, stageName, trialSubject, code = {
          if (dbSubject.stages.get(stageName).isEmpty || dbSubject.stages.get(stageName).get.isEmpty) {
            if (trial.stages.get(stageName).isDefined) {
              val stage = trial.stages.get(stageName).get
              //TODO check single properties
              if (stage.size == properties.size) {
                trialSubjectDao.addStage(trialSubject, stageName, properties)
              } else {
                Failure("Subject properties doesn't match stage criterions")
              }
            } else {
              Failure("The stage (" + stageName + ") is not part of the trial (" + trial.name + ")")
            }
          } else {
            Failure("Trial stage (" + stageName + ") exists for this trial subject.")
          }
        })
      } else {
        Failure("Trial is not ACTIVE")
      }
    }

    private def getStages(trial: Trial): Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]] = {
      if (trial.randomizationMethod.isDefined) {
        val plugin = randomizationPluginManager.getPluginForMethod(trial.randomizationMethod.get)
        if (plugin.isDefined) {
          plugin.get.randomizationConfigurationOptions()._2
        } else {
          Map()
        }
      } else {
        Map()
      }
    }

  }

}
