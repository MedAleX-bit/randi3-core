package liquibase.database.ext

import liquibase.database.core.H2Database

class H2DatabaseCaseSensitive extends H2Database {

  override def escapeDatabaseObject(objectName: String): String = {
     "\""+objectName+"\""
  }
}
