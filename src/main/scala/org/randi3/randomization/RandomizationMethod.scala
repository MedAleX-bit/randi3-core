package org.randi3.randomization

import org.randi3.model._
import org.apache.commons.math3.random._

abstract class RandomizationMethod extends Entity {

  val random: RandomGenerator

  def randomize(trial: Trial, subject: TrialSubject): TreatmentArm
}
