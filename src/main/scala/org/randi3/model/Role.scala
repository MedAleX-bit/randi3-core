package org.randi3.model

final object Role extends Enumeration {
  val principleInvestigator = Value("Principle Investigator")
  val trialAdministrator = Value("Trial Administrator")
  val investigator = Value("Investigator")
  val monitor = Value("Monitor")
  val statistician = Value("Statistician")
}