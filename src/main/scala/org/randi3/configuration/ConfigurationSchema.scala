package org.randi3.configuration

import scala.slick.session.Database
import scala.slick.session.Database.threadLocalSession

import java.util.Properties
import org.randi3.utility.Logging

import slick.driver.{HsqldbDriver, ExtendedProfile}
import HsqldbDriver.simple._

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
      "jdbc:hsqldb:mem:configurationRANDI2"
    else
      "jdbc:hsqldb:file:"+ path +"configurationRANDI2"

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
    db withSession {
      ConfigurationProperties.ddl.create
    }

    (db, scala.slick.driver.HsqldbDriver)
  }


  def getDatabase(databaseJDBC: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL(databaseJDBC)
    (db, scala.slick.driver.HsqldbDriver)
  }

}
