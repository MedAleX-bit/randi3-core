package org.randi3.configuration


import scalaz._
import Scalaz._
import java.sql.{Connection, SQLException}
import org.randi3.utility.Utility
import scala.slick.lifted.Parameters
import scalaz.Digit._2
import org.randi3.configuration.ConfigurationValues._
import scalaz.Failure
import scalaz.Success
import scala.slick.session.Database._
import org.randi3.schema.SupportedDatabases

trait ConfigurationServiceComponent  {

  this: Utility =>

  val configurationService: ConfigurationService

class ConfigurationService {

  val databaseTuple = ConfigurationSchema.getDatabase
  val database = databaseTuple._1

  import databaseTuple._2.Implicit._
  import ConfigurationSchema._


  private val queryConfigurationByName = for {
    name <- Parameters[String]
    conf <- ConfigurationProperties if conf.propertyName is name
  } yield conf.propertyName ~ conf.propertyValue


  def saveConfigurationEntry(configurationType: String, value: String): Validation[String, Boolean] = {
    try {
      database withSession {
        val entry = queryConfigurationByName(configurationType)

        val size = entry.list().size

        //new value
        if (size == 0) {
          threadLocalSession withTransaction {
            ConfigurationProperties insert(configurationType, value)
          }
          Success(true)

        } else if (size == 1) {
          threadLocalSession withTransaction {
            entry.mutate(r => r.row = r.row.copy(_2 = value))
          }
          Success(true)
        } else {
          Failure("dublicated entry")
        }

      }
    } catch {
      case e: SQLException => logError(e); Failure("Configuration database error: " + e.getMessage)
      case e: Throwable => Failure(logError(e))
    }
  }

  def getConfigurationEntry(configurationType: String): Validation[String, String] = {
    try {
      database withSession {
        val entry = queryConfigurationByName(configurationType).list()

        if (entry.size == 1) {
          Success(entry.head._2)

        } else if (entry.size == 0) {
          Failure("Entry not found")
        } else {
          Failure("Duplicated entry")
        }

      }
    } catch {
      case e: SQLException => logError(e); Failure("Configuration database error: " + e.getMessage)
      case e: Throwable => Failure(logError(e))
    }
  }

  def isConfigurationComplete: Boolean = {

    getConfigurationEntry(DB_TYPE.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(DB_ADDRESS.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(DB_USER.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(DB_PASSWORD.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(DB_NAME.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }
    getConfigurationEntry(MAIL_SERVER.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(MAIL_FROM.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(MAIL_PORT.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(MAIL_SMTP_AUTH.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(MAIL_SSL.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(MAIL_USERNAME.toString).toEither match {
      case Left(failure) => return false
      case Right(b) =>
    }

    getConfigurationEntry(MAIL_PASSWORD.toString).toEither match {
      case Left(failure) => return false
      case Right(b) =>
    }

    getConfigurationEntry(PLUGIN_PATH.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }

    getConfigurationEntry(INITIAL_OBJECTS_CREATED.toString).toEither match {
      case Left(failure) => return false
      case Right(b) => if (b.isEmpty) return false
    }


    true
  }

  def upDateDatabase(jdbcConnection: Connection): Boolean = {

    //    val liquibase = new Liquibase("basic_schema_db.xml", new ClassLoaderResourceAccessor(this.getClass.getClassLoader), new JdbcConnection(jdbcConnection))
    //
    //    liquibase.update("")

    false
  }

}
}

object ConfigurationService {

  def generateJDBCURL(db_type: String, db_address: String, db_user: String, db_password: String, db_name: String): String = {
    val result = new StringBuffer()
    //"jdbc:mysql://localhost/randi3?user=randi3&password=randi3&sessionVariables=storage_engine=InnoDB"
    result.append("jdbc:")
    result.append(db_type.toLowerCase)
    result.append("://")
    result.append(db_address)
    result.append("/")
    result.append(db_name)
    result.append("?")
    result.append("user=")
    result.append(db_user)
    result.append("&")
    result.append("password=")
    result.append(db_password)

    if (SupportedDatabases.MySQL.toString == db_type)
    result.append("&sessionVariables=storage_engine=InnoDB")

    result.toString
  }
}
