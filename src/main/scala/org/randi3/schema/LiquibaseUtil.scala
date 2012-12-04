package org.randi3.schema

import java.sql.{SQLException, Connection}
import liquibase.Liquibase
import org.randi3.dao.DaoComponent
import liquibase.database.{DatabaseFactory, Database}
import liquibase.resource.ClassLoaderResourceAccessor
import liquibase.database.jvm.JdbcConnection
import org.scalaquery.session

class LiquibaseUtil {

  def updateDatabase(database: session.Database) {
    val c: Connection = database.createSession().conn
    val databaseLiquibase: Database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(c))

    val liquibase = new Liquibase("db/db.changelog-master.xml", new ClassLoaderResourceAccessor(), databaseLiquibase)

    liquibase.update(null)

    }
}
