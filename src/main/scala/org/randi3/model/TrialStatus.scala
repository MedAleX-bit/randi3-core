package org.randi3.model

object TrialStatus extends Enumeration {

  val  	/**
   * The trial is active, subjects can be randomized. Only the description and
   * the end date of the study can be edited. New protocol can be uploaded.
   */
  ACTIVE,

  /**
   * The trial is stored in the system but no randomization is allowed. Any
   * property of the trial (as well as any property of the randomization
   * algorithm) can be changed.
   */
  IN_PREPARATION,
  /**
   * No randomization and changes to the trial possible.
   */
  FINISHED,
  /**
   * Similar to ACTIVE but the randomization process is not allowed.
   */
  PAUSED = Value
}
