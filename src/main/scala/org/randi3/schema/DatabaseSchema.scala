package org.randi3.schema

// Import the session management, including the implicit threadLocalSession

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.ql.extended._
import java.sql.Blob
import java.sql.Date
import java.sql.Timestamp

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object DatabaseSchema {

  val Trials = new Table[(Int, Int, String, String, String, Date, Date, String, String, String)]("Trials") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def name = column[String]("Name", O NotNull)

    def abbreviation = column[String]("Abbreviation", O NotNull)

    def description = column[String]("Description")

    def startDate = column[Date]("StartDate")

    def endDate = column[Date]("EndDate")

    def stratifyTrialSite = column[String]("StratifyTrialSite")

    def status = column[String]("Status")

    def subjectIdentificationCreationType = column[String]("SubjectIdentificationCreationType")

    //    def randomizationMethodID = column[Option[Int]]("RandomizationMethodID", O Nullable)
    def * = id ~ version ~ name ~ abbreviation ~ description ~ startDate ~ endDate ~ stratifyTrialSite ~ status ~ subjectIdentificationCreationType

    def noId = version ~ name ~ abbreviation ~ description ~ startDate ~ endDate ~ stratifyTrialSite ~ status ~ subjectIdentificationCreationType

    //   def randomizationMethod = foreignKey("RandomizationMethodFK", randomizationMethodID, RandomizationMethods)(_.id)
    def uniqueName = index("uniqueTrialName", name, unique = true)
  }

  val TreatmentArms = new Table[(Int, Int, String, String, Int, Int)]("TreatmentArms") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def name = column[String]("Name", O NotNull)

    def description = column[String]("Description", O NotNull)

    def trialId = column[Int]("TrialID")

    def plannedSize = column[Int]("PlannedSize")

    def * = id ~ version ~ name ~ description ~ trialId ~ plannedSize

    def noId = version ~ name ~ description ~ trialId ~ plannedSize

    def trial = foreignKey("TrialFK_TreatmentArms", trialId, Trials)(_.id)
  }

  val TrialSubjects = new Table[(Int, Int, Timestamp, Int, String, String, Int)]("TrialSubjects") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def createdAt = column[Timestamp]("CreatedAt", O NotNull)

    def treatmentArmId = column[Int]("TreatmentArmId")

    def identifier = column[String]("Identifier", O NotNull)

    def investigatorUserName = column[String]("InvestigatorUserName", O NotNull)

    def trialSiteId = column[Int]("TrialSiteId", O NotNull)

    def * = id ~ version ~ createdAt ~ treatmentArmId ~ identifier ~ investigatorUserName ~ trialSiteId

    def noId = version ~ createdAt ~ treatmentArmId ~ identifier ~ investigatorUserName ~ trialSiteId

    def treatmentArm = foreignKey("TreatmentArmFK_TrialSubject", treatmentArmId, TreatmentArms)(_.id)

    def trialSite = foreignKey("TrialSiteFK_TrialSubject", trialSiteId, TrialSites)(_.id)
  }

  val RandomizationMethods = new Table[(Option[Int], Int, Option[Blob], String)]("RandomizationMethod") {
    def id = column[Option[Int]]("ID", O PrimaryKey, O AutoInc)

    def trialId = column[Int]("trialId")

    def randomGenerator = column[Option[Blob]]("RandomGenerator", O NotNull)

    def randomizationType = column[String]("RandomizationType", O NotNull)

    def * = id ~ trialId ~ randomGenerator ~ randomizationType

    def noId = trialId ~ randomGenerator ~ randomizationType

    def trial = foreignKey("trialFK_randomizaition", trialId, Trials)(_.id)
  }

  val TrialSites = new Table[(Int, Int, String, String, String, String, String, String)]("TrialSites") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def name = column[String]("Name", O NotNull)

    def country = column[String]("Country", O NotNull)

    def postCode = column[String]("PostCode", O NotNull)

    def city = column[String]("City", O NotNull)

    def street = column[String]("Street", O NotNull)

    def password = column[String]("Password", O NotNull)

    def * = id ~ version ~ name ~ country ~ postCode ~ city ~ street ~ password

    def noId = version ~ name ~ country ~ postCode ~ city ~ street ~ password

    def uniqueName = index("uniqueTrialSiteName", name, unique = true)
  }

  val ParticipatingSites = new Table[(Int, Int)]("ParticipatingSites") {
    def trialId = column[Int]("TrialId", O NotNull)

    def trialSiteId = column[Int]("TrialSiteId", O NotNull)

    def * = trialId ~ trialSiteId

    def pk = primaryKey("PK_ParticipatingSites", trialId ~ trialSiteId)

    def trial = foreignKey("participationFK_Trial", trialId, Trials)(_.id)

    def trialSite = foreignKey("participationFK_TrialSite", trialSiteId, TrialSites)(_.id)
  }

  val Users = new Table[(Int, Int, String, String, String, String, String, Int, String, Boolean, Boolean)]("Users") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def username = column[String]("UserName", O NotNull)

    def email = column[String]("EMail", O NotNull)

    def firstName = column[String]("FirstName")

    def lastName = column[String]("LastName")

    def phoneNumber = column[String]("PhoneNumber")

    def siteId = column[Int]("siteId", O NotNull)

    def password = column[String]("password", O NotNull)

    def administrator = column[Boolean]("Administrator", O NotNull)

    def canCreateTrials = column[Boolean]("CanCreateTrials", O NotNull)

    def * = id ~ version ~ username ~ email ~ firstName ~ lastName ~ phoneNumber ~ siteId ~ password ~ administrator ~ canCreateTrials

    def noId = version ~ username ~ email ~ firstName ~ lastName ~ phoneNumber ~ siteId ~ password ~ administrator ~ canCreateTrials

    def trialSite = foreignKey("TrialSiteFK_Users", siteId, TrialSites)(_.id)

    def uniqueUserName = index("uniqueUsername", username, unique = true)
  }

  val Rights = new Table[(Int, Int, String)]("Rights") {
    def userId = column[Int]("UserID")

    def trialId = column[Int]("TrialId")

    def role = column[String]("Role")

    def * = userId ~ trialId ~ role

    def pk = primaryKey("pk_rights", userId ~ trialId ~ role)

    def trial = foreignKey("trialFK_Rights", trialId, Trials)(_.id)

    def user = foreignKey("UserFK_Rights", userId, Users)(_.id)
  }

  val Criterions = new Table[(Int, Int, Int, String, String, String, Option[Int])]("Criterions") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def trialId = column[Int]("TrialId")

    def name = column[String]("Name")

    def description = column[String]("Description")

    def criterionType = column[String]("CriterionType", O NotNull)

    def inclusionConstraintId = column[Option[Int]]("InclusionConstraint")

    def * = id ~ version ~ trialId ~ name ~ description ~ criterionType ~ inclusionConstraintId

    def noId = version ~ trialId ~ name ~ description ~ criterionType ~ inclusionConstraintId

    def trial = foreignKey("trialFK_Criterions", trialId, Trials)(_.id)

    def inclusionConstraint = foreignKey("inclusionConstraintFK_Criterions", inclusionConstraintId.get, Constraints)(_.id)
  }

  val OrdinalCriterionValues = new Table[(Int, Int, String)]("OrdinalCriterionValues") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def criterionId = column[Int]("CriterionId")

    def value = column[String]("Value")

    def * = id ~ criterionId ~ value

    def noId = criterionId ~ value

    def criterion = foreignKey("CriterionFK_Value", criterionId, Criterions)(_.id)
  }

  val Strata = new Table[(Int, Int, Int, Int)]("Strata") {
    def id = column[Int]("Id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def criterionId = column[Int]("CriterionId", O NotNull)

    def constraintId = column[Int]("ConstraintId", O NotNull)

    def * = id ~ version ~ criterionId ~ constraintId

    def noId = version ~ criterionId ~ constraintId
  }


  val Constraints = new Table[(Int, Int, String, Option[String], Option[Date], Option[Date], Option[Double], Option[Double], Option[Int], Option[Int], String)]("Constraints") {
    def id = column[Int]("Id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def constraintType = column[String]("ConstraintType", O NotNull)

    def expectedValueFreeText = column[Option[String]]("ExpectedValueFreeText")

    def firstDate = column[Option[Date]]("FirstDate")

    def secondDate = column[Option[Date]]("SecondDate")

    def firstDouble = column[Option[Double]]("FirstDouble")

    def secondDouble = column[Option[Double]]("SecondDouble")

    def firstInteger = column[Option[Int]]("FirstInteger")

    def secondInteger = column[Option[Int]]("SecondInteger")

    def uuid = column[String]("UuidOnlyForInternalUse")

    def * = id ~ version ~ constraintType ~ expectedValueFreeText ~ firstDate ~ secondDate ~ firstDouble ~ secondDouble ~ firstInteger ~ secondInteger ~ uuid

    def noId = version ~ constraintType ~ expectedValueFreeText ~ firstDate ~ secondDate ~ firstDouble ~ secondDouble ~ firstInteger ~ secondInteger ~ uuid

    def uniqueUUID = index("uniqueConstraintUUID", uuid, unique = true)
  }

  val OrdinalConstraintValues = new Table[(Int, Int, Int, String)]("OrdinalConstraintValues") {
    def id = column[Int]("Id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def constraintId = column[Int]("ConstraintId", O NotNull)

    def value = column[String]("Value")

    def * = id ~ version ~ constraintId ~ value

    def noId = version ~ constraintId ~ value

    def constraint = foreignKey("constraintFK_OrdinalConstraint", constraintId, Constraints)(_.id)
  }

  val TrialStages = new Table[(Int, Int, String, Int)]("TrialStages") {
    def id = column[Int]("Id", O PrimaryKey, O AutoInc)

    def trialId = column[Int]("TrialId", O NotNull)

    def stageName = column[String]("StageName", O NotNull)

    def criterionId = column[Int]("CriterionId")

    def * = id ~ trialId ~ stageName ~ criterionId

    def noId = trialId ~ stageName ~ criterionId

    def criterion = foreignKey("CriterionFK_stages", criterionId, Criterions)(_.id)

    def trial = foreignKey("TrialFK_stages", trialId, Trials)(_.id)
  }

  val SubjectProperties = new Table[(Int, Int, Int, Int, Option[Date], Option[String], Option[Int], Option[Double])]("SubjectProperties") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def criterionId = column[Int]("CriterionId")

    def subjectId = column[Int]("SubjectId")

    def dateValue = column[Option[Date]]("DateValue")

    def stringValue = column[Option[String]]("StringValue")

    def intValue = column[Option[Int]]("IntValue")

    def doubleValue = column[Option[Double]]("DoubleValue")

    def * = id ~ version ~ criterionId ~ subjectId ~ dateValue ~ stringValue ~ intValue ~ doubleValue

    def noId = version ~ criterionId ~ subjectId ~ dateValue ~ stringValue ~ intValue ~ doubleValue

    def criterion = foreignKey("SubjectPropertyFK_Criterion", criterionId, Criterions)(_.id)

    def subject = foreignKey("SubjectPropertyFK_Subject", subjectId, TrialSubjects)(_.id)
  }

  val Audit = new Table[(Int, Timestamp, String, String, String, Int, String)]("Audit") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def time = column[Timestamp]("Time", O NotNull)

    def username = column[String]("Username", O NotNull)

    def action = column[String]("Action", O NotNull)

    def clazz = column[String]("Class", O NotNull)

    def identifier = column[Int]("Identifier", O NotNull)

    def text = column[String]("Text", O NotNull)

    def * = id ~ time ~ username ~ action ~ clazz ~ identifier ~ text

    def noId = time ~ username ~ action ~ clazz ~ identifier ~ text
  }


  private def createDatabaseTables(db: Database, driver: ExtendedProfile) {
    import driver.Implicit._
    db withSession {
      (Trials.ddl ++
        TreatmentArms.ddl ++
        TrialSubjects.ddl ++
        RandomizationMethods.ddl ++
        TrialSites.ddl ++
        Users.ddl ++
        Rights.ddl ++
        Criterions.ddl ++
        OrdinalCriterionValues.ddl ++
        SubjectProperties.ddl ++
        Constraints.ddl ++
        OrdinalConstraintValues.ddl ++
        TrialStages.ddl ++
        Strata.ddl ++
        ParticipatingSites.ddl ++
        Audit.ddl).create
    }
  }

  def createDatabaseH2(databaseName: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:h2:mem:" + databaseName + ";DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=100000")
    createDatabaseTables(db, org.scalaquery.ql.extended.H2Driver)
    (db, org.scalaquery.ql.extended.H2Driver)
  }

  def getDatabaseH2(databaseName: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:h2:mem:" + databaseName + ";DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=100000")
    (db, org.scalaquery.ql.extended.H2Driver)
  }

  def getDatabaseMySql: (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:mysql://localhost/randi3?user=randi3&password=randi3&sessionVariables=storage_engine=InnoDB")
    (db, org.scalaquery.ql.extended.MySQLDriver)
  }


  def createDatabaseMySql(jdbcURL: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL(jdbcURL)
    createDatabaseTables(db, org.scalaquery.ql.extended.MySQLDriver)
    (db, org.scalaquery.ql.extended.MySQLDriver)
  }


  def createDatabaseMySql: (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:mysql://localhost/randi3?user=randi3&password=randi3&sessionVariables=storage_engine=InnoDB")
    createDatabaseTables(db, org.scalaquery.ql.extended.MySQLDriver)
    (db, org.scalaquery.ql.extended.MySQLDriver)
  }

  //at the moment problems with the blob types
  def createDatabasePostgres: (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:postgresql://localhost/randi3?user=randi3&password=randi3")
    createDatabaseTables(db, org.scalaquery.ql.extended.PostgresDriver)
    (db, org.scalaquery.ql.extended.PostgresDriver)
  }

  def main(args: Array[String]) {
    import org.scalaquery.ql.extended.MySQLDriver.Implicit._
    // Connect to the database and execute the following block within a session
    Database.forURL("jdbc:mysql://localhost/randi3?user=randi3&password=randi3") withSession {

      // Database.forURL("jdbc:postgresql://localhost/randi3?user=randi3&password=randi3") withSession {
      //      Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver") withSession {
      // The session is never named explicitly. It is bound to the current
      // thread as the threadLocalSession that we imported
      (Trials.ddl ++ TreatmentArms.ddl ++ TrialSubjects.ddl ++ RandomizationMethods.ddl).create

    }
  }

}
