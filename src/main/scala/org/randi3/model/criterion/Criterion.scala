package org.randi3.model.criterion

import org.randi3.model.Entity
import org.randi3.model.criterion.constraint.Constraint

abstract class Criterion[T, +V <: Constraint[T]] extends Entity {

  val name: String
  val description: String

  val strata: List[V]
  val inclusionConstraint: Option[V]

  def isValueCorrect(value: T): Boolean = {
    val constraint = inclusionConstraint.getOrElse(return true)
    constraint.isValueCorrect(value)
  }

  def stratify(value: T): Option[V] = {
    strata.foreach(stratum => if (stratum.isValueCorrect(value)) return Some(stratum))
    None
  }
}
