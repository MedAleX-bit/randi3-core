package org.randi3.model.criterion.constraint

import org.randi3.model.Entity._
import scalaz._
import Scalaz._

abstract class FreeTextConstraint extends Constraint[String] {

  val id: Int
  val version: Int
  val configurations:List[Option[String]]

}