package org.randi3.randomization

import org.randi3.randomization.configuration._
import org.randi3.model._
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.apache.commons.math3.random._
import org.scalaquery.ql._
import org.scalaquery.session.Database
import org.scalaquery.ql.extended.ExtendedProfile
import scalaz._


abstract class RandomizationMethodPlugin(database: Database, driver: ExtendedProfile) {

  val name: String

  val i18nName: String

  val description: String

  val canBeUsedWithStratification: Boolean

  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], List[Criterion[_ <: Any, Constraint[_ <: Any]]])

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]]

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod]

  def databaseTables(): Option[DDL]

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int]

  def get(id: Int): Validation[String, Option[RandomizationMethod]]

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]]

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod]

  def delete(randomizationMethod: RandomizationMethod)

}
