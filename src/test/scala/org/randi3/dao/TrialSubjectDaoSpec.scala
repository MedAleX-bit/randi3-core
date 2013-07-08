package org.randi3.dao

import java.util.Date

import org.junit.runner.RunWith
import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.schema.DatabaseSchema._
import scala.slick.lifted.Query
import org.randi3.utility.TestingEnvironment

import scala.slick.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers

import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model._
import scala.collection.mutable.ListBuffer
import org.joda.time.{LocalDate, DateTime}

@RunWith(classOf[JUnitRunner])
class TrialSubjectDaoSpec extends FunSpec with MustMatchers {

  import TestingEnvironment._
  import schema._
  import TestingEnvironment.driver.Implicit._

  describe("The TrialSubjectDao create method") {

    it("should be able to create a trial subject with a identifier, subject properties (FreeText, Date, Integer, Double, Ordinal Criterion) and a corresponding treatment arm") {

      val trialDB = createTrialWithOneTreatmentAndAllCriterions
      val trialSite = trialDB.participatingSites.head
      val subjectProperties = createSubjectProperties(trialDB)

      trialDB.criterions.size must be(subjectProperties.size)

      val subjectId = trialSubjectDao.create(TrialSubject(createdAt = new DateTime(), identifier = "identifier", investigatorUserName = "username", trialSite = trialSite, properties = subjectProperties.toList, stages = Map()).toOption.get, trialDB.treatmentArms(0).id).toEither match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      subjectId must be > Int.MinValue

      database withSession {
        val allTrialSubjects = Query(TrialSubjects).list
        val subjectEntry = allTrialSubjects.filter(entry => entry._1 == subjectId)
        subjectEntry.size must be(1)
        subjectEntry.head._1 must be(subjectId)
        subjectEntry.head._4 must be(trialDB.treatmentArms(0).id)
        subjectEntry.head._5 must be("identifier")

        val subjectPropertiesDB = Query(SubjectProperties).list.filter(entry => entry._4 == subjectId)

        subjectPropertiesDB.size must be(subjectProperties.size)

        subjectPropertiesDB.foreach {
          property =>
            trialDB.criterions.map(criterion => criterion.id).contains(property._3) must be(true)
            val criterion = trialDB.criterions.filter(criterionDB => criterionDB.id == property._3)(0)
            val subjectPropertyValue = subjectProperties.toList.filter(prop => prop.criterion.getClass == criterion.getClass)(0).value
            if (criterion.getClass == classOf[DateCriterion]) {
              property._5.isDefined must be(true)
              //TODO date
              //property._5.get must be(subjectProperyValue)
              property._6 must be(None)
              property._7 must be(None)
              property._8 must be(None)
            } else if (criterion.getClass == classOf[DoubleCriterion]) {
              property._5 must be(None)
              property._6 must be(None)
              property._7 must be(None)
              property._8.isDefined must be(true)
              property._8.get must be(subjectPropertyValue)
            } else if (criterion.getClass == classOf[IntegerCriterion]) {
              property._5 must be(None)
              property._6 must be(None)
              property._7.isDefined must be(true)
              property._7.get must be(subjectPropertyValue)
              property._8 must be(None)
            } else if (criterion.getClass == classOf[FreeTextCriterion]) {
              property._5 must be(None)
              property._6.isDefined must be(true)
              property._6.get must be(subjectPropertyValue)
              property._7 must be(None)
              property._8 must be(None)
            } else if (criterion.getClass == classOf[OrdinalCriterion]) {
              property._5 must be(None)
              property._6.isDefined must be(true)
              property._6.get must be(subjectPropertyValue)
              property._7 must be(None)
              property._8 must be(None)
            } else fail("property type not found")
        }
      }

    }

  }

  describe("The TrialSubjectDao get(id) method") {

    it("should be able to get a trial subject with a specific id, including the identifier and the properties") {

      val trialDB = createTrialWithOneTreatmentAndAllCriterions
      val trialSite = trialDB.participatingSites.head

      val subjectProperties = createSubjectProperties(trialDB)

      val subject = TrialSubject(identifier = "identifier", investigatorUserName = "username", trialSite = trialSite, properties = subjectProperties).toOption.get
      val subjectId = trialSubjectDao.create(subject, trialDB.treatmentArms(0).id).toEither match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val subjectDB = trialSubjectDao.get(subjectId).toEither match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial subject not found")
        case Right(Some(x)) => x
      }

      val propertyComparisonList = subjectProperties.toList.map(property => (property.criterion, property.value))

      subjectDB.id must be(subjectId)
      subjectDB.identifier must be(subject.identifier)

      trialDB.criterions.size must be(5)

      subjectDB.properties.size must be(subjectProperties.size)
      subjectDB.properties.foreach {
        property =>
          property.id must be > 0
          //TODO date
          if (property.criterion.getClass != classOf[DateCriterion])
            propertyComparisonList.contains((property.criterion, property.value)) must be(true)
      }
    }

  }

  describe("The TrialSubjectDao get(identifier) method") {

    it("should be able to get a trial subject with a specific identifier") {

      val trialDB = createTrialWithOneTreatmentAndAllCriterions
      val trialSite = trialDB.participatingSites.head

      val subjectProperties = createSubjectProperties(trialDB)

      val subject = TrialSubject(identifier = "identifier", investigatorUserName = "username", trialSite = trialSite, properties = subjectProperties).toOption.get
      val subjectId = trialSubjectDao.create(subject, trialDB.treatmentArms(0).id).toEither match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val subjectDB = trialSubjectDao.get(subject.identifier, trialDB.id).toEither match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial subject not found")
        case Right(Some(x)) => x
      }

      val propertyComparisonList = subjectProperties.toList.map(property => (property.criterion, property.value))

      subjectDB.id must be(subjectId)
      subjectDB.identifier must be(subject.identifier)

      subjectDB.properties.size must be(subjectProperties.size)
      subjectDB.properties.foreach {
        property =>
          property.id must be > 0
          //TODO date
          if (property.criterion.getClass != classOf[DateCriterion])
            propertyComparisonList.contains((property.criterion, property.value)) must be(true)
      }
    }

  }

  describe("The TrialSubjectDao update method") {

    it("should be able to update the identifier") {
      val trialDB = createTrialWithOneTreatmentAndAllCriterions
      val trialSite = trialDB.participatingSites.head

      val subjectDB = trialSubjectDao.get(trialSubjectDao.create(TrialSubject(identifier = "identifier", investigatorUserName = "username", trialSite = trialSite, properties = List()).toOption.get, trialDB.treatmentArms(0).id).toOption.get).toOption.get.get

      val changedSubject = subjectDB.copy(identifier = "changedIdentifier")

      trialSubjectDao.update(changedSubject)

      val changedSubjectDB = trialSubjectDao.get(subjectDB.id).toEither match {
        case Left(x) => fail(x)
        case Right(None) => fail("trial subject not found")
        case Right(Some(x)) => x
      }

      changedSubjectDB.id must be(subjectDB.id)
      changedSubjectDB.identifier must be(changedSubject.identifier)
      changedSubjectDB.properties must be(subjectDB.properties)
    }

    it("should be able to update the subject properties")(pending)

    it("should be able to update the subject outcome")(pending)

  }

  describe("The TrialSubjectDao getAllFromTreatmentArm method") {

    it("should return all trial subjects of the specified treatment arm") {
      val trialDB = createTrialDB
      val trialSite = trialDB.participatingSites.head
      val treatmentArmDB1 = trialDB.treatmentArms(0)

      val treatmentArmDB2 = trialDB.treatmentArms(1)

      val idsSubjectsArm1 = new ListBuffer[Int]

      for (i <- 1 until 11) {
        idsSubjectsArm1 += trialSubjectDao.create(TrialSubject(identifier = "identifier" + i, investigatorUserName = "username", trialSite = trialSite, properties = List()).toOption.get, treatmentArmDB1.id).toOption.get
      }

      val idsSubjectsArm2 = new ListBuffer[Int]
      for (i <- 11 until 31) {
        idsSubjectsArm2 += trialSubjectDao.create(TrialSubject(identifier = "identifier" + i, investigatorUserName = "username", trialSite = trialSite, properties = List()).toOption.get, treatmentArmDB2.id).toOption.get
      }

      val subjectsArm1 = trialSubjectDao.getAllFromTreatmentArm(treatmentArmDB1.id).toEither match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      subjectsArm1.size must be(10)
      for (subject <- subjectsArm1) {
        idsSubjectsArm1.contains(subject.id)
      }

      val subjectsArm2 = trialSubjectDao.getAllFromTreatmentArm(treatmentArmDB2.id).toEither match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      subjectsArm2.size must be(20)

      for (subject <- subjectsArm2) {
        idsSubjectsArm2.contains(subject.id)
      }

    }
  }

  describe("The TrialSubjectDao getAllFromTrial method") {

    it("should return all trial subjects of the specified trial")(pending)
  }

  private def createTrialWithOneTreatmentAndAllCriterions: Trial = {
    val freeTextCriterion = FreeTextCriterion(name = "freeText", description = "description", inclusionConstraint = None, strata = Nil).toOption.get
    val integerCriterion = IntegerCriterion(name = "integer", description = "description", inclusionConstraint = None, strata = Nil).toOption.get
    val doubleCriterion = DoubleCriterion(name = "double", description = "description", inclusionConstraint = None, strata = Nil).toOption.get
    val dateCriterion = DateCriterion(name = "date", description = "description", inclusionConstraint = None, strata = Nil).toOption.get
    val ordinalCriterion = OrdinalCriterion(name = "ordinal", description = "description", inclusionConstraint = None, strata = Nil, values = Set("a", "b", "c")).toOption.get
    val criterions = List(freeTextCriterion, integerCriterion, doubleCriterion, dateCriterion, ordinalCriterion)
    val trialId = trialDao.create(createTrial.copy(criterions = criterions, participatingSites = List(createTrialSiteDB))).toOption.get

    trialDao.get(trialId).toOption.get.get
  }

  private def createSubjectProperties(trialDB: Trial): List[SubjectProperty[Any]] = {
    val subjectProperties = new ListBuffer[SubjectProperty[Any]]()

    trialDB.criterions.foreach {
      actCriterion =>
        val crit = actCriterion.asInstanceOf[Criterion[Any, Constraint[Any]]]
        subjectProperties.append({
          if (actCriterion.getClass == classOf[FreeTextCriterion]) SubjectProperty(criterion = crit, value = "subjectPropertyValue")
          else if (actCriterion.getClass == classOf[IntegerCriterion]) SubjectProperty(criterion = crit, value = 1)
          else if (actCriterion.getClass == classOf[DoubleCriterion]) SubjectProperty(criterion = crit, value = 1.0)
          else if (actCriterion.getClass == classOf[DateCriterion]) SubjectProperty(criterion = crit, value = new LocalDate)
          else if (actCriterion.getClass == classOf[OrdinalCriterion]) SubjectProperty(criterion = crit, value = "a")
          else throw new RuntimeException("criterion not found")
        }.toOption.get)
    }
    subjectProperties.toList
  }

}
