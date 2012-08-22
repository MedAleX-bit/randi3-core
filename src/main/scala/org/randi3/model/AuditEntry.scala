package org.randi3.model

import org.joda.time.DateTime

case class AuditEntry(id: Int = Int.MinValue, time: DateTime, username: String, action: ActionType.Value, clazz: Class[Any], identifier: Int, text: String) {

}
