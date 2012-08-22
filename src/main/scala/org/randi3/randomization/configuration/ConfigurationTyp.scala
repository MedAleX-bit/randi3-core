package org.randi3.randomization.configuration

import org.randi3.model.Entity

abstract class ConfigurationType[+T] {

  val name: String
  val description: String
}

case class IntegerConfigurationType(name: String, description: String) extends ConfigurationType[Int] {}

case class DoubleConfigurationType(name: String, description: String) extends ConfigurationType[Double] {}

case class BooleanConfigurationType(name: String, description: String) extends ConfigurationType[Boolean] {}

case class OrdinalConfigurationType(name: String, description: String, options: Set[String]) extends ConfigurationType[String]
