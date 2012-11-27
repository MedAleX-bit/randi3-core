package org.randi3.dao

import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database
import org.randi3.schema.DatabaseSchema

trait DaoComponent {

  val database: Database
  val driver: ExtendedProfile
  val schema: DatabaseSchema
}