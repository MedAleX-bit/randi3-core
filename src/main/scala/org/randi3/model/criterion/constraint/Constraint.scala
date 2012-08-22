package org.randi3.model.criterion.constraint

import org.randi3.model.Entity

trait Constraint[T] extends Entity {

  val configurations: List[Option[T]]

  def isValueCorrect(value: T): Boolean
}
