package org.randi3.dao

import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database

trait DaoComponent {

  val database: Database
  val driver: ExtendedProfile

}