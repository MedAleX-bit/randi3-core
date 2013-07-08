package org.randi3.dao

import org.randi3.randomization._
import scala.slick.session.Database.threadLocalSession
import scalaz._
import org.randi3.utility.UtilityDBComponent
import scala.slick.session.Session

trait RandomizationMethodDaoComponent {

  this: DaoComponent with
    RandomizationPluginManagerComponent with
    UtilityDBComponent =>

  val randomizationMethodDao: RandomizationMethodDao

  class RandomizationMethodDao extends AbstractRandomizationMethodDao(database, driver) {

    import utilityDB._


    def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int] = {
      val randomizationMethodPlugin = randomizationPluginManager.getPlugin(randomizationMethod.getClass.getName).getOrElse(return Failure("Plugin not found"))
      //check if a method exists
      getFromTrialId(trialId).toEither match {
        case Left(x) => return Failure(x)
        case Right(Some(method)) => return Failure("Method already exists")
        case _ =>
      }
      //TODO
      if (randomizationMethod.id > 0) Failure("Method already exists")
      else randomizationMethodPlugin.create(randomizationMethod, trialId)
    }

    def get(id: Int): Validation[String, Option[RandomizationMethod]] = {
      onDB {
        threadLocalSession withTransaction {
        val resultList = queryRandomizationMethodFromId(id).list
        if (resultList.isEmpty) Success(None)
        else if (resultList.size == 1) randomizationPluginManager.getPlugin(resultList(0)._4).getOrElse(return Failure("Plugin not found")).get(id)

        else Failure("More than one method with id=" + id + " found")
      }
      }
    }

    def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]] = {
      onDB {
        threadLocalSession withTransaction {
        val resultList = queryRandomizationMethodFromTrialId(trialId).list
        if (resultList.isEmpty) Success(None)
        else if (resultList.size == 1) randomizationPluginManager.getPlugin(resultList(0)._4).getOrElse(return Failure("Plugin not found")).getFromTrialId(resultList(0)._2)
        else Failure("More than one method with for trial (id=" + trialId + ") found")
      }
      }
    }

    def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod] = {
      val randomizationMethodPlugin = randomizationPluginManager.getPlugin(randomizationMethod.getClass.getName).getOrElse(return Failure("Plugin not found"))
      randomizationMethodPlugin.update(randomizationMethod)
    }

    def delete (randomizationMethod: RandomizationMethod): Validation[String, Boolean] = {
      val randomizationMethodPlugin = randomizationPluginManager.getPlugin(randomizationMethod.getClass.getName).getOrElse(return Failure("Plugin not found"))
      randomizationMethodPlugin.delete(randomizationMethod)
      Success(true)
    }
  }

}
