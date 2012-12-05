package org.randi3.schema

import java.sql.{SQLException, Connection}
import liquibase.Liquibase
import org.randi3.dao.DaoComponent
import liquibase.database.{DatabaseFactory, Database}
import liquibase.resource.ClassLoaderResourceAccessor
import liquibase.database.jvm.JdbcConnection
import org.scalaquery.session

object LiquibaseUtil {

  def updateDatabase(database: session.Database) {
    updateDatabase(database, "db/db.changelog-master.xml")
  }

  def updateDatabase(database: session.Database, changelog: String) {
    updateDatabase(database, "db/db.changelog-master.xml", this.getClass.getClassLoader)
  }

  def updateDatabase(database: session.Database, changelog: String, classloader: ClassLoader) {
    val c: Connection = database.createSession().conn
    val databaseLiquibase: Database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(c))

    val liquibase = new Liquibase(changelog, new ClassLoaderResourceAccessor(classloader), databaseLiquibase)

    liquibase.update(null)
  }
}
