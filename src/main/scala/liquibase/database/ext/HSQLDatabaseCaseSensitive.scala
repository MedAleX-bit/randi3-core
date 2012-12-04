package liquibase.database.ext

import liquibase.database.core.HsqlDatabase

class HSQLDatabaseCaseSensitive extends HsqlDatabase {

  override def escapeDatabaseObject(objectName: String): String = {
     "\""+objectName+"\""
  }
}
