package org.randi3.utility

import org.apache.log4j.Logger

trait LogUtility {
  val loggerName = this.getClass.getName
  lazy val logger = Logger.getLogger(loggerName)
}
