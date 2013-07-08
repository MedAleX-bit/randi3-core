package org.randi3.dao

import org.randi3.schema.DatabaseSchema
import scala.slick.driver.ExtendedProfile
import slick.session.Database

trait DaoComponent {

  val database: Database
  val driver: ExtendedProfile
  val schema: DatabaseSchema
}