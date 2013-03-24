package org.randi3.schema

// Import the session management, including the implicit threadLocalSession

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.ExtendedProfile
import java.sql.Blob
import java.sql.Date
import java.sql.Timestamp
import org.scalaquery.ql.ForeignKeyAction

import org.scalaquery.ql.extended.{ExtendedTable => Table}


class DatabaseSchema(val driver: ExtendedProfile) {

  import driver.Implicit._

  object Trials extends Table[(Int, Int, String, String, String, Date, Date, String, String, Boolean, Boolean, Boolean)]("Trials") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def name = column[String]("Name", O NotNull)

    def abbreviation = column[String]("Abbreviation", O NotNull)

    def description = column[String]("Description")

    def startDate = column[Date]("StartDate")

    def endDate = column[Date]("EndDate")

    def status = column[String]("Status")

    def subjectIdentificationCreationType = column[String]("SubjectIdentificationCreationType")

    def isEDCTrial = column[Boolean]("isEDCTrial", O NotNull)

    def isTrialOpen = column[Boolean]("isTrialOpen", O NotNull)

    def isStratifiedByTrialSite = column[Boolean]("isStratifiedByTrialSite", O NotNull)

    //    def randomizationMethodID = column[Option[Int]]("RandomizationMethodID", O Nullable)
    def * = id ~ version ~ name ~ abbreviation ~ description ~ startDate ~ endDate ~ status ~ subjectIdentificationCreationType ~ isEDCTrial ~ isTrialOpen ~ isStratifiedByTrialSite

    def noId = version ~ name ~ abbreviation ~ description ~ startDate ~ endDate ~ status ~ subjectIdentificationCreationType ~ isEDCTrial ~ isTrialOpen ~ isStratifiedByTrialSite

    //   def randomizationMethod = foreignKey("RandomizationMethodFK", randomizationMethodID, RandomizationMethods)(_.id)
    def uniqueName = index("uniqueTrialName", name, unique = true)
  }

  object TreatmentArms extends Table[(Int, Int, String, String, Int, Int)]("TreatmentArms") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def name = column[String]("Name", O NotNull)

    def description = column[String]("Description", O NotNull)

    def trialId = column[Int]("TrialID")

    def plannedSize = column[Int]("PlannedSize")

    def * = id ~ version ~ name ~ description ~ trialId ~ plannedSize

    def noId = version ~ name ~ description ~ trialId ~ plannedSize

    def trial = foreignKey("TrialFK_TreatmentArms", trialId, Trials)(_.id)
  }

  object TrialSubjects extends Table[(Int, Int, Timestamp, Int, String, String, Int)]("TrialSubjects") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

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

  object RandomizationMethods extends Table[(Option[Int], Int, Array[Byte], String, Long)]("RandomizationMethod") {
    def id = column[Option[Int]]("id", O PrimaryKey, O AutoInc)

    def trialId = column[Int]("trialId")

    def randomGenerator = column[Array[Byte]]("RandomGenerator", O NotNull)(PostgresByteArrayTypeMapper)

    def randomizationType = column[String]("RandomizationType", O NotNull)

    def seed = column[Long]("Seed")

    def * = id ~ trialId ~ randomGenerator ~ randomizationType ~ seed

    def noId = trialId ~ randomGenerator ~ randomizationType ~ seed

    def trial = foreignKey("trialFK_randomizaition", trialId, Trials)(_.id)
  }

  object TrialSites extends Table[(Int, Int, String, String, String, String, String, String, Boolean)]("TrialSites") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def name = column[String]("Name", O NotNull)

    def country = column[String]("Country", O NotNull)

    def postCode = column[String]("PostCode", O NotNull)

    def city = column[String]("City", O NotNull)

    def street = column[String]("Street", O NotNull)

    def password = column[String]("Password", O NotNull)

    def isActive = column[Boolean]("isActive", O NotNull)

    def * = id ~ version ~ name ~ country ~ postCode ~ city ~ street ~ password ~ isActive

    def noId = version ~ name ~ country ~ postCode ~ city ~ street ~ password ~ isActive

    def uniqueName = index("uniqueTrialSiteName", name, unique = true)
  }

  object ParticipatingSites extends Table[(Int, Int)]("ParticipatingSites") {
    def trialId = column[Int]("TrialId", O NotNull)

    def trialSiteId = column[Int]("TrialSiteId", O NotNull)

    def * = trialId ~ trialSiteId

    def pk = primaryKey("PK_ParticipatingSites", trialId ~ trialSiteId)

    def trial = foreignKey("participationFK_Trial", trialId, Trials)(_.id)

    def trialSite = foreignKey("participationFK_TrialSite", trialSiteId, TrialSites)(_.id)
  }

  object Users extends Table[(Int, Int, String, String, String, String, String, Int, String, Boolean, Boolean, Boolean, Int, Option[Timestamp], Option[Date], String)]("Users") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

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

    def isActive = column[Boolean]("isActive", O NotNull)

    def numberOfFailedLogins = column[Int]("numberOfFailedLogins", O NotNull)

    def lockedUntil = column[Option[Timestamp]]("lockedUntil")

    def passwordExpiresAt = column[Option[Date]]("passwordExpiresAt")

    def locale = column[String]("locale", O NotNull)

    def * = id ~ version ~ username ~ email ~ firstName ~ lastName ~ phoneNumber ~ siteId ~ password ~ administrator ~ canCreateTrials ~ isActive ~ numberOfFailedLogins ~ lockedUntil ~ passwordExpiresAt ~ locale

    def noId = version ~ username ~ email ~ firstName ~ lastName ~ phoneNumber ~ siteId ~ password ~ administrator ~ canCreateTrials ~ isActive ~ numberOfFailedLogins ~ lockedUntil ~ passwordExpiresAt ~ locale

    def trialSite = foreignKey("TrialSiteFK_Users", siteId, TrialSites)(_.id)

    def uniqueUserName = index("uniqueUsername", username, unique = true)
  }

  object Rights extends Table[(Int, Int, String)]("Rights") {
    def userId = column[Int]("UserID")

    def trialId = column[Int]("TrialId")

    def role = column[String]("Role")

    def * = userId ~ trialId ~ role

    def pk = primaryKey("pk_rights", userId ~ trialId ~ role)

    def trial = foreignKey("trialFK_Rights", trialId, Trials)(_.id)

    def user = foreignKey("UserFK_Rights", userId, Users)(_.id)
  }

  object Criterions extends Table[(Int, Int, Int, String, String, String, Option[Int])]("Criterions") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

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

  object OrdinalCriterionValues extends Table[(Int, Int, String)]("OrdinalCriterionValues") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def criterionId = column[Int]("CriterionId")

    def value = column[String]("Value")

    def * = id ~ criterionId ~ value

    def noId = criterionId ~ value

    def criterion = foreignKey("CriterionFK_Value", criterionId, Criterions)(_.id, onDelete = ForeignKeyAction.Cascade)
  }

  object Strata extends Table[(Int, Int, Int, Int)]("Strata") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def criterionId = column[Int]("CriterionId", O NotNull)

    def constraintId = column[Int]("ConstraintId", O NotNull)

    def * = id ~ version ~ criterionId ~ constraintId

    def noId = version ~ criterionId ~ constraintId
  }


  object Constraints extends Table[(Int, Int, String, Option[String], Option[Date], Option[Date], Option[Double], Option[Double], Option[Int], Option[Int], String)]("Constraints") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

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

  object OrdinalConstraintValues extends Table[(Int, Int, Int, String)]("OrdinalConstraintValues") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def constraintId = column[Int]("ConstraintId", O NotNull)

    def value = column[String]("Value")

    def * = id ~ version ~ constraintId ~ value

    def noId = version ~ constraintId ~ value

    def constraint = foreignKey("constraintFK_OrdinalConstraint", constraintId, Constraints)(_.id, onDelete = ForeignKeyAction.Cascade)


  }

  object TrialStages extends Table[(Int, Int, String, Int)]("TrialStages") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def trialId = column[Int]("TrialId", O NotNull)

    def stageName = column[String]("StageName", O NotNull)

    def criterionId = column[Int]("CriterionId")

    def * = id ~ trialId ~ stageName ~ criterionId

    def noId = trialId ~ stageName ~ criterionId

    def criterion = foreignKey("CriterionFK_stages", criterionId, Criterions)(_.id)

    def trial = foreignKey("TrialFK_stages", trialId, Trials)(_.id)
  }

  object SubjectProperties extends Table[(Int, Int, Int, Int, Option[Date], Option[String], Option[Int], Option[Double])]("SubjectProperties") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

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

  object Audit extends Table[(Int, Timestamp, String, String, String, Int, String)]("Audit") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def time = column[Timestamp]("Time", O NotNull)

    def username = column[String]("Username", O NotNull)

    def action = column[String]("Action", O NotNull)

    def clazz = column[String]("Class", O NotNull)

    def identifier = column[Int]("Identifier", O NotNull)

    def text = column[String]("Text", O NotNull)

    def * = id ~ time ~ username ~ action ~ clazz ~ identifier ~ text

    def noId = time ~ username ~ action ~ clazz ~ identifier ~ text
  }


}

object DatabaseSchema {

  def schema(driver: ExtendedProfile): DatabaseSchema =  new DatabaseSchema(driver)


  def getDatabaseH2(databaseName: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:h2:mem:" + databaseName + ";DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=100000")
    (db, org.scalaquery.ql.extended.H2Driver)
  }

  def getDatabaseHSqlDB(databaseName: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:hsqldb:mem:"+ databaseName +"")
    (db, org.scalaquery.ql.extended.HsqldbDriver)
  }

  def getDatabaseMySql(databaseName: String, user: String, password: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:mysql://localhost/"+databaseName+"?user="+ user + "&password=" + password + "&sessionVariables=storage_engine=InnoDB")
    (db, org.scalaquery.ql.extended.MySQLDriver)
  }

  def getDatabasePostgreSQL(databaseName: String, user: String, password: String): (Database, ExtendedProfile) = {
    val db: Database = Database.forURL("jdbc:postgresql://localhost/"+databaseName+"?user="+ user + "&password=" + password)   //
    (db, org.scalaquery.ql.extended.PostgresDriver)
  }

}
