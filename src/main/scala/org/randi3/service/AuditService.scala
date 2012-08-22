package org.randi3.service


import org.randi3.dao.AuditDaoComponent
import org.randi3.model.{Entity, AuditEntry, Trial}
import scalaz._

trait AuditServiceComponent {
  this: AuditDaoComponent =>

  val auditService: AuditService

  class AuditService {

      def getAudit(element: Entity): Validation[String, List[AuditEntry]] = {
         //TODO security check
        auditDao.getAll(element.getClass.asInstanceOf[Class[Any]], element.id)
      }

    def getAudit(username: String): Validation[String, List[AuditEntry]] = {
      //TODO security check
      auditDao.getAll(username)
    }
  }

}
