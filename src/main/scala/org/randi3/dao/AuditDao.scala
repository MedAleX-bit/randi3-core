package org.randi3.dao

import scalaz._
import Scalaz._
import org.scalaquery.session.Database._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.randi3.schema.DatabaseSchema._
import java.sql.Timestamp
import org.randi3.utility._
import collection.mutable.ListBuffer
import java.util.Date
import org.randi3.model.{ActionType, AuditEntry}
import org.joda.time.DateTime


trait AuditDaoComponent {

  this: DaoComponent with
    Logging with
    UtilityDBComponent =>

  val auditDao: AuditDao

  class AuditDao {

    import driver.Implicit._
    import utilityDB._

    private val queryAuditEntriesFromClassAndIdentifier = for {
      Projection(clazz, identifier) <- Parameters[String, Int]
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
          resultList += new AuditEntry(row._1, new DateTime(row._2.getTime), row._3, ActionType.withName(row._4), clazz, row._6, row._7)
      }
      resultList.toList.success
    }

  }

}
