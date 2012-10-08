package org.randi3.configuration

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.ql.extended._

import java.io.FileInputStream
import java.io.BufferedInputStream
import java.util.Properties
import org.randi3.utility.Logging

object ConfigurationSchema extends Logging {

  val databaseURL = {

    val properties = new Properties()
    logger.info("Try to load config file ...")
    val stream = getClass.getClassLoader.getResourceAsStream("config.properties")
    logger.info("Config file loaded!")
    properties.load(stream)
    stream.close()
    val path = properties.getProperty("configPath")
    val inMemoryDB = properties.getProperty("inMemoryConfigDB").toBoolean
    val url = if (inMemoryDB)
      "jdbc:h2:mem:configurationRANDI2;DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=100000"
    else
      "jdbc:h2:file:" + path + "configurationRANDI2;DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=100000"

    logger.info("Created jdbc-url for the configuration database (" + url + ")!")
    url
  }


  val ConfigurationProperties = new Table[(String, String)]("ConfigurationProperties") {
    def propertyName = column[String]("propertyName", O PrimaryKey, O NotNull)

    def propertyValue = column[String]("propertyValue", O NotNull)

    def * = propertyName ~ propertyValue
  }


  def createDatabase: (Database, ExtendedProfile) = {
    createDatabase(databaseURL)
  }

  def getDatabase: (Database, ExtendedProfile) = {
    getDatabase(databaseURL)
  }

  def createDatabase(databaseJDBC: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL(databaseJDBC)
    import org.scalaquery.ql.extended.H2Driver.Implicit._

    db withSession {
      ConfigurationProperties.ddl.create
    }

    (db, org.scalaquery.ql.extended.H2Driver)
  }


  def getDatabase(databaseJDBC: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL(databaseJDBC)
    (db, org.scalaquery.ql.extended.H2Driver)
  }

}
