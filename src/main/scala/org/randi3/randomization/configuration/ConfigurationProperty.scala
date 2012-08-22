package org.randi3.randomization.configuration

case class ConfigurationProperty[T](configurationType: ConfigurationType[T], value: T) {

}
