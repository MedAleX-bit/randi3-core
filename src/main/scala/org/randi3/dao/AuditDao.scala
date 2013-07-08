package org.randi3.dao

import scalaz._
import Scalaz._
import scala.slick.session.Database.threadLocalSession
import java.sql.Timestamp
import org.randi3.utility._
import collection.mutable.ListBuffer
import org.randi3.model.{ActionType, AuditEntry}
import org.joda.time.DateTime
import scala.slick.lifted.Parameters

trait AuditDaoComponent {

  this: DaoComponent with
    Logging with
    UtilityDBComponent with
    I18NComponent=>

  val auditDao: AuditDao

  class AuditDao {

    import driver.Implicit._
    import schema._
    import utilityDB._
    import i18n._

    private def queryAuditEntriesFromClassAndIdentifier(clazz: String, identifier: Int) = for {
        auditEntry <- Audit if auditEntry.clazz === clazz && auditEntry.identifier === identifier
    } yield auditEntry

    private val queryAuditEntriesFromUsername = for {
      username <- Parameters[String]
      auditEntry <- Audit if auditEntry.username === username
    } yield auditEntry


    def create(auditEntry: AuditEntry): Validation[String, Boolean] = {
      logger.info(auditEntry.toString)
      onDB {
        threadLocalSession withTransaction {
          Audit.noId insert(new Timestamp(auditEntry.time.getMillis), auditEntry.username, auditEntry.action.toString, auditEntry.clazz.getName, auditEntry.identifier, auditEntry.text)
        }
        Success(true)
      }
    }

    def getAll(clazz: Class[Any], identifier: Int): Validation[String, List[AuditEntry]] = {
      onDB {
        generateAuditEntryFromDatabaseRows(queryAuditEntriesFromClassAndIdentifier(clazz.getName, identifier).list)
      }
    }

    def getAll(username: String): Validation[String, List[AuditEntry]] = {
      onDB {
        generateAuditEntryFromDatabaseRows(queryAuditEntriesFromUsername(username).list)
      }
    }

    private def generateAuditEntryFromDatabaseRows(rows: List[(Int, Timestamp, String, String, String, Int, String)]): Validation[String, List[AuditEntry]] = {
      //TODO check and uiName
      val resultList = new ListBuffer[AuditEntry]()
      rows.foreach {
        row =>
          val clazz = getClass.getClassLoader.loadClass(row._5).asInstanceOf[Class[Any]]
          resultList += new AuditEntry(row._1, new DateTime(row._2.getTime), row._3, ActionType.withName(row._4), clazz, row._6, text(row._7))
      }
      resultList.toList.success
    }

  }

}
