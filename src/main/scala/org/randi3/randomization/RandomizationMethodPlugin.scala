package org.randi3.randomization

import org.randi3.randomization.configuration._
import org.randi3.model._
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.apache.commons.math3.random._

import scala.slick.session.Database
import scalaz._

import org.randi3.utility.{AbstractSecurityUtil, SecurityComponent}
import scala.slick.driver.ExtendedProfile
import scala.slick.lifted.DDL


abstract class RandomizationMethodPlugin(database: Database, driver: ExtendedProfile, securityUtil: AbstractSecurityUtil){

  val name: String

  def i18nName: String

  def description: String

  val canBeUsedWithStratification: Boolean

  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]])

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]]

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod]

  def databaseTables(): Option[DDL]

  def updateDatabase()

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int]

  def get(id: Int): Validation[String, Option[RandomizationMethod]]

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]]

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod]

  def delete(randomizationMethod: RandomizationMethod)

}
