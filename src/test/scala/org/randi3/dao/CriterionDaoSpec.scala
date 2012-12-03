package org.randi3.dao

import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.scalaquery.ql.Query
import org.scalaquery.ql.extended.H2Driver.Implicit._

import org.scalaquery.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import org.joda.time.LocalDate

//TODO Don't repeat yourself
@RunWith(classOf[JUnitRunner])
class CriterionDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._
  import schema._

  describe("The CriterionDao create method") {

    it("should be able to create a FreeTextCriterion") {
      val trialDB = createTrialDB
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
      allCriterions.head._7 must be(None)

    }

    it("should be able to create a FreeTextCriterion with inclusion constraint (FreeTextConstraintExact)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(FreeTextConstraintExact(configurations = List(Some("expectedValue"))).toOption.get)
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
      allCriterions.head._7.isDefined must be(true)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4.isDefined must be(true)
      inclusionConstraintsDB(0)._4.get must be("expectedValue")
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)

    }

    it("should be able to create a FreeTextCriterion with inclusion constraint (FreeTextConstraintNotEmpty)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(FreeTextConstraintNotEmpty().toOption.get)
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
      allCriterions.head._7.isDefined must be(true)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)

    }

    it("should be able to create a FreeTextCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(FreeTextConstraintExact(configurations = List(Some("expectedValue"))).toOption.get, FreeTextConstraintExact(configurations = List(Some("expectedValue2"))).toOption.get)
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
      allCriterions.head._7 must be(None)

      val strataDB = database withSession {
        Query(Strata).list.filter(strata => strata._3 == id)
      }

      strataDB.size must be(strata.size)

      val constraintIDs = strataDB.map(strata => strata._4)
      val constraintsDB = database withSession {
        Query(Constraints).list.filter(constraint => constraintIDs.contains(constraint._1))
      }

      constraintsDB.size must be(strata.size)

      val expectedValues = strata.map(constraint => constraint.expectedValue)
      constraintsDB.foreach {
        constraint =>
          constraint._3 must be(strata(0).getClass.getName)
          constraint._4.isDefined must be(true)
          expectedValues.contains(constraint._4.get) must be(true)
      }

    }

    it("should be able to create a DateCriterion") {
      val trialDB = createTrialDB
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
    }

    it("should be able to create a DateCriterion with inclusion constraint (first value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DateConstraint(configurations = List(None, Some(new LocalDate))).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6.isDefined must be(true)
      //TODO date
      //inclusionConstraintsDB(0)._6 must be(inclusionConstraint.get.secondValue)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a DateCriterion with inclusion constraint (second value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DateConstraint(configurations = List(Some(new LocalDate), None)).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5.isDefined must be(true)
      //TODO date
      //  inclusionConstraintsDB(0)._5 must be(inclusionConstraint.get.firstValue)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a DateCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DateConstraint(configurations = List(Some(new LocalDate), Some(new LocalDate))).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)
      //TODO date 
      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5.isDefined must be(true)
      //  inclusionConstraintsDB(0)._5 must be(inclusionConstraint.get.firstValue)
      inclusionConstraintsDB(0)._6.isDefined must be(true)
      //  inclusionConstraintsDB(0)._6 must be(inclusionConstraint.get.secondValue)     
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a DateCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        DateConstraint(configurations = List(Some(new LocalDate), None)).toOption.get,
        DateConstraint(configurations = List(None, Some(new LocalDate))).toOption.get,
        DateConstraint(configurations = List(Some(new LocalDate), Some(new LocalDate))).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val strataDB = database withSession {
        Query(Strata).list.filter(strata => strata._3 == id)
      }

      strataDB.size must be(strata.size)

      val constraintIDs = strataDB.map(strata => strata._4)
      val constraintsDB = database withSession {
        Query(Constraints).list.filter(constraint => constraintIDs.contains(constraint._1))
      }

      constraintsDB.size must be(strata.size)

      val expectedValues = strata.map(constraint => (constraint.firstValue, constraint.secondValue))
      //TODO date
      constraintsDB.foreach {
        constraint =>
          constraint._3 must be(strata(0).getClass.getName)
        // expectedValues.contains((constraint._5, constraint._6)) must be(true) 
      }
    }

    it("should be able to create a DoubleCriterion") {
      val trialDB = createTrialDB
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
    }

    it("should be able to create a DoubleCriterion with inclusion constraint (first value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DoubleConstraint(configurations = List(None, Some(1.0))).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8.isDefined must be(true)
      inclusionConstraintsDB(0)._8 must be(inclusionConstraint.get.secondValue)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a DoubleCriterion with inclusion constraint (second value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DoubleConstraint(configurations = List(Some(1.0), None)).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7.isDefined must be(true)
      inclusionConstraintsDB(0)._7 must be(inclusionConstraint.get.firstValue)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a DoubleCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DoubleConstraint(configurations = List(Some(1.0), Some(2.0))).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7.isDefined must be(true)
      inclusionConstraintsDB(0)._7 must be(inclusionConstraint.get.firstValue)
      inclusionConstraintsDB(0)._8.isDefined must be(true)
      inclusionConstraintsDB(0)._8 must be(inclusionConstraint.get.secondValue)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a DoubleCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        DoubleConstraint(configurations = List(Some(1.2), None)).toOption.get,
        DoubleConstraint(configurations = List(None, Some(2.3))).toOption.get,
        DoubleConstraint(configurations = List(Some(4.1), Some(5.8))).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val strataDB = database withSession {
        Query(Strata).list.filter(strata => strata._3 == id)
      }

      strataDB.size must be(strata.size)

      val constraintIDs = strataDB.map(strata => strata._4)
      val constraintsDB = database withSession {
        Query(Constraints).list.filter(constraint => constraintIDs.contains(constraint._1))
      }

      constraintsDB.size must be(strata.size)
      val expectedValues = strata.map(constraint => (constraint.firstValue, constraint.secondValue))
      constraintsDB.foreach {
        constraint =>
          constraint._3 must be(strata(0).getClass.getName)
          expectedValues.contains((constraint._7, constraint._8)) must be(true)
      }
    }

    it("should be able to create a IntegerCriterion") {
      val trialDB = createTrialDB
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)
    }

    it("should be able to create a IntegerCriterion with inclusion constraint (first value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(IntegerConstraint(configurations = List(None, Some(1))).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10.isDefined must be(true)
      inclusionConstraintsDB(0)._10 must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to create a IntegerCriterion with inclusion constraint (second value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(IntegerConstraint(configurations = List(Some(1), None)).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9.isDefined must be(true)
      inclusionConstraintsDB(0)._9 must be(inclusionConstraint.get.firstValue)
      inclusionConstraintsDB(0)._10 must be(None)
    }

    it("should be able to create a IntegerCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(IntegerConstraint(configurations = List(Some(1), Some(2))).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9.isDefined must be(true)
      inclusionConstraintsDB(0)._9 must be(inclusionConstraint.get.firstValue)
      inclusionConstraintsDB(0)._10.isDefined must be(true)
      inclusionConstraintsDB(0)._10 must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to create a IntegerCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        IntegerConstraint(configurations = List(Some(1), None)).toOption.get,
        IntegerConstraint(configurations = List(None, Some(2))).toOption.get,
        IntegerConstraint(configurations = List(Some(5), Some(8))).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val strataDB = database withSession {
        Query(Strata).list.filter(strata => strata._3 == id)
      }

      strataDB.size must be(strata.size)

      val constraintIDs = strataDB.map(strata => strata._4)
      val constraintsDB = database withSession {
        Query(Constraints).list.filter(constraint => constraintIDs.contains(constraint._1))
      }

      constraintsDB.size must be(strata.size)

      val expectedValues = strata.map(constraint => (constraint.firstValue, constraint.secondValue))
      constraintsDB.foreach {
        constraint =>
          constraint._3 must be(strata(0).getClass.getName)
          expectedValues.contains((constraint._9, constraint._10)) must be(true)
      }
    }

    it("should be able to create a OrdinalCriterion") {
      val trialDB = createTrialDB
      val criterion: OrdinalCriterion = OrdinalCriterion(name = "name", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val ordinalValuesDB = database withSession {
        Query(OrdinalCriterionValues).list.filter(entry => entry._2 == id).map(entry => entry._3)
      }

      ordinalValuesDB.size must be(3)
      ordinalValuesDB.containsSlice(criterion.values.toSeq) must be(true)

    }

    it("should be able to create a OrdinalCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(OrdinalConstraint(configurations = List(Some("value1"), Some("value2"))).toOption.get)
      val criterion: OrdinalCriterion = OrdinalCriterion(name = "name", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val ordinalValuesDB = database withSession {
        Query(OrdinalCriterionValues).list.filter(entry => entry._2 == id).map(entry => entry._3)
      }

      ordinalValuesDB.size must be(3)
      ordinalValuesDB.containsSlice(criterion.values.toSeq) must be(true)

      val allConstraints = database withSession {
        Query(Constraints).list
      }

      val inclusionConstraintsDB = allConstraints.filter(c => c._1 == allCriterions.head._7.get)

      inclusionConstraintsDB.size must be(1)

      inclusionConstraintsDB(0)._3 must be(inclusionConstraint.get.getClass.getName)
      inclusionConstraintsDB(0)._4 must be(None)
      inclusionConstraintsDB(0)._5 must be(None)
      inclusionConstraintsDB(0)._6 must be(None)
      inclusionConstraintsDB(0)._7 must be(None)
      inclusionConstraintsDB(0)._8 must be(None)
      inclusionConstraintsDB(0)._9 must be(None)
      inclusionConstraintsDB(0)._10 must be(None)

      val ordinalConstraintValuesDB = database withSession {
        Query(OrdinalConstraintValues).list.filter(entry => entry._3 == inclusionConstraintsDB(0)._1).map(entry => entry._4)
      }

      ordinalConstraintValuesDB.size must be(2)
      ordinalConstraintValuesDB.containsSlice(inclusionConstraint.get.expectedValues.toSeq) must be(true)

    }

    it("should be able to create a OrdinalCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        OrdinalConstraint(configurations = List(Some("value1"))).toOption.get,
        OrdinalConstraint(configurations = List(Some("value2"))).toOption.get,
        OrdinalConstraint(configurations = List(Some("value1"), Some("value2"))).toOption.get)
      val criterion: OrdinalCriterion = OrdinalCriterion(name = "name", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val allCriterions = getCriterionsDataRowsFromTrial(trialDB.id)

      allCriterions.size must be(1)

      allCriterions.head._1 must be(id)
      allCriterions.head._3 must be(trialDB.id)
      allCriterions.head._4 must be(criterion.name)
      allCriterions.head._5 must be(criterion.description)
      allCriterions.head._6 must be(criterion.getClass.getName)

      val ordinalValuesDB = database withSession {
        Query(OrdinalCriterionValues).list.filter(entry => entry._2 == id).map(entry => entry._3)
      }

      ordinalValuesDB.size must be(3)
      ordinalValuesDB.containsSlice(criterion.values.toSeq) must be(true)

      val strataDB = database withSession {
        Query(Strata).list.filter(strata => strata._3 == id)
      }

      strataDB.size must be(strata.size)

      val constraintIDs = strataDB.map(strata => strata._4)
      val constraintsDB = database withSession {
        Query(Constraints).list.filter(constraint => constraintIDs.contains(constraint._1))
      }

      constraintsDB.size must be(strata.size)

      val expectedValues = strata.map(constraint => constraint.expectedValues.toList)
      constraintsDB.foreach {
        constraint =>
          constraint._3 must be(strata(0).getClass.getName)
          val ordinalValuesDB = database withSession {
            Query(OrdinalConstraintValues).list.filter(entry => entry._3 == constraint._1).map(entry => entry._4)
          }
          expectedValues.contains(ordinalValuesDB) must be(true)
      }

    }

  }

  describe("The CriterionDao get method") {

    it("should be able to get a FreeTextCriterion without inclusion constraint") {
      val trialDB = createTrialDB
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)
    }

    it("should be able to get a FreeTextCriterion with inclusion constraint (FreeTextConstraintExact)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(FreeTextConstraintExact(configurations = List(Some("expectedValue"))).toOption.get)
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[FreeTextConstraintExact].expectedValue must be(inclusionConstraint.get.expectedValue)
    }

    it("should be able to get a FreeTextCriterion with inclusion constraint (FreeTextConstraintNotEmpty)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(FreeTextConstraintNotEmpty().toOption.get)
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
    }

    it("should be able to get a FreeTextCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(FreeTextConstraintExact(configurations = List(Some("expectedValue"))).toOption.get, FreeTextConstraintExact(configurations = List(Some("expectedValue2"))).toOption.get)
      val criterion: FreeTextCriterion = FreeTextCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)

      criterionDB.strata.size must be(strata.size)

      val expectedValues = strata.map(constraint => constraint.expectedValue)
      criterionDB.strata.foreach {
        constraint =>
          constraint.getClass must be(strata(0).getClass)
          expectedValues.contains(constraint.asInstanceOf[FreeTextConstraintExact].expectedValue) must be(true)
      }
    }

    it("should be able to get a DateCriterion without inclusion constraint") {
      val trialDB = createTrialDB
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)
    }

    it("should be able to get a DateCriterion with inclusion constraint (first value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DateConstraint(configurations = List(None, Some(new LocalDate))).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[DateConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      //TODO date mapping
      //      criterionDB.inclusionConstraint.get.asInstanceOf[DateConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a DateCriterion with inclusion constraint (second value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DateConstraint(configurations = List(Some(new LocalDate), None)).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      //TODO date mapping
      //      criterionDB.inclusionConstraint.get.asInstanceOf[DateConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[DateConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a DateCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DateConstraint(configurations = List(Some(new LocalDate), Some(new LocalDate))).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      //TODO date mapping
      //      criterionDB.inclusionConstraint.get.asInstanceOf[DateConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      //      criterionDB.inclusionConstraint.get.asInstanceOf[DateConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a DateCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        DateConstraint(configurations = List(Some(new LocalDate), None)).toOption.get,
        DateConstraint(configurations = List(None, Some(new LocalDate))).toOption.get,
        DateConstraint(configurations = List(Some(new LocalDate), Some(new LocalDate))).toOption.get)
      val criterion: DateCriterion = DateCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)

      criterionDB.strata.size must be(strata.size)

      val expectedValues = strata.map(constraint => (constraint.firstValue, constraint.secondValue))
      //TODO date
      criterionDB.strata.foreach {
        constraint =>
          constraint.getClass must be(strata(0).getClass)
          val actConstraint = constraint.asInstanceOf[DateConstraint]
        //        expectedValues.contains((actConstraint.firstValue, actConstraint.secondValue)) must be(true)
      }
    }

    it("should be able to get a DoubleCriterion without inclusion constraint") {
      val trialDB = createTrialDB
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)
    }

    it("should be able to get a DoubleCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DoubleConstraint(configurations = List(Some(1.0), Some(2.0))).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[DoubleConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[DoubleConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a DoubleCriterion with inclusion constraint (first value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DoubleConstraint(configurations = List(None, Some(2.0))).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[DoubleConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[DoubleConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a DoubleCriterion with inclusion constraint (second value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(DoubleConstraint(configurations = List(Some(1.0), None)).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[DoubleConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[DoubleConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a DoubleCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        DoubleConstraint(configurations = List(Some(1.2), None)).toOption.get,
        DoubleConstraint(configurations = List(None, Some(2.3))).toOption.get,
        DoubleConstraint(configurations = List(Some(4.1), Some(5.8))).toOption.get)
      val criterion: DoubleCriterion = DoubleCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)

      criterionDB.strata.size must be(strata.size)

      val expectedValues = strata.map(constraint => (constraint.firstValue, constraint.secondValue))
      criterionDB.strata.foreach {
        constraint =>
          constraint.getClass must be(strata(0).getClass)
          val actConstraint = constraint.asInstanceOf[DoubleConstraint]
          expectedValues.contains((actConstraint.firstValue, actConstraint.secondValue)) must be(true)
      }
    }

    it("should be able to get a IntegerCriterion without inclusion criterion") {
      val trialDB = createTrialDB
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)
    }

    it("should be able to get a IntegerCriterion with inclusion criterion") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(IntegerConstraint(configurations = List(Some(3), Some(5))).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[IntegerConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[IntegerConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a IntegerCriterion with inclusion criterion (first value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(IntegerConstraint(configurations = List(None, Some(5))).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[IntegerConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[IntegerConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a IntegerCriterion with inclusion criterion (second value None)") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(IntegerConstraint(configurations = List(Some(3), None)).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[IntegerConstraint].firstValue must be(inclusionConstraint.get.firstValue)
      criterionDB.inclusionConstraint.get.asInstanceOf[IntegerConstraint].secondValue must be(inclusionConstraint.get.secondValue)
    }

    it("should be able to get a IntegerCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        IntegerConstraint(configurations = List(Some(2), None)).toOption.get,
        IntegerConstraint(configurations = List(None, Some(2))).toOption.get,
        IntegerConstraint(configurations = List(Some(4), Some(5))).toOption.get)
      val criterion: IntegerCriterion = IntegerCriterion(name = "name", description = "descritpion", inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)

      criterionDB.strata.size must be(strata.size)

      val expectedValues = strata.map(constraint => (constraint.firstValue, constraint.secondValue))
      criterionDB.strata.foreach {
        constraint =>
          constraint.getClass must be(strata(0).getClass)
          val actConstraint = constraint.asInstanceOf[IntegerConstraint]
          expectedValues.contains((actConstraint.firstValue, actConstraint.secondValue)) must be(true)
      }
    }

    it("should be able to get a OrdinalCriterion without inclusion constraint") {
      val trialDB = createTrialDB
      val criterion: OrdinalCriterion = OrdinalCriterion(name = "name", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = None, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)

      val dbValues = criterionDB.asInstanceOf[OrdinalCriterion].values
      val values = criterion.asInstanceOf[OrdinalCriterion].values

      dbValues.size must be(values.size)
      dbValues.foreach(v => values.contains(v) must be(true))
    }

    it("should be able to get a OrdinalCriterion with inclusion constraint") {
      val trialDB = createTrialDB
      val inclusionConstraint = Some(OrdinalConstraint(configurations = List(Some("value1"), Some("value2"))).toOption.get)
      val criterion: OrdinalCriterion = OrdinalCriterion(name = "name", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)

      val dbValues = criterionDB.asInstanceOf[OrdinalCriterion].values
      val values = criterion.asInstanceOf[OrdinalCriterion].values

      dbValues.size must be(values.size)
      dbValues.foreach(v => values.contains(v) must be(true))

      criterionDB.inclusionConstraint.isDefined must be(true)
      criterionDB.inclusionConstraint.get.getClass must be(inclusionConstraint.get.getClass)
      criterionDB.inclusionConstraint.get.asInstanceOf[OrdinalConstraint].expectedValues must be(inclusionConstraint.get.expectedValues)
    }

    it("should be able to get a OdinalCriterion with strata") {
      val trialDB = createTrialDB
      val strata = List(
        OrdinalConstraint(configurations = List(Some("value1"))).toOption.get,
        OrdinalConstraint(configurations = List(Some("value2"))).toOption.get,
        OrdinalConstraint(configurations = List(Some("value1"), Some("value2"))).toOption.get)
      val criterion: OrdinalCriterion = OrdinalCriterion(name = "name", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = None, strata = strata).toOption.get
      val id = criterionDao.create(criterion, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      val criterionDB = criterionDao.get(id).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      criterionDB must not be (null)
      criterionDB.id must be(id)
      criterionDB.name must be(criterion.name)
      criterionDB.description must be(criterion.description)
      criterionDB.getClass must be(criterion.getClass)
      criterionDB.inclusionConstraint must be(None)

      criterionDB.strata.size must be(strata.size)

      val expectedValues = strata.map(constraint => constraint.expectedValues)
      criterionDB.strata.foreach {
        constraint =>
          constraint.getClass must be(strata(0).getClass)
          expectedValues.contains(constraint.asInstanceOf[OrdinalConstraint].expectedValues) must be(true)
      }
    }

  }

  describe("The CriterionDao getCriterions method with a trial id") {

    it("should be able to get all criterion of a specified trial") {
      val trialDB = createTrialDB
      val criterion1: FreeTextCriterion = FreeTextCriterion(name = "name1", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
      val criterion2: OrdinalCriterion = OrdinalCriterion(name = "name2", description = "descritpion", values = Set("value1", "value2", "value3"), inclusionConstraint = None, strata = Nil).toOption.get
      val criterion3: FreeTextCriterion = FreeTextCriterion(name = "name3", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get

      val criterion1DB = criterionDao.get(criterionDao.create(criterion1, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      val criterion2DB = criterionDao.get(criterionDao.create(criterion2, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }
      val criterion3DB = criterionDao.get(criterionDao.create(criterion3, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }).either match {
        case Left(x) => fail(x)
        case Right(None) => fail("criterion not found")
        case Right(Some(x)) => x
      }

      val criterionsDB = criterionDao.getCriterions(trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(x) => x
      }

      criterionsDB.size must be(3)
      criterionsDB.contains(criterion1DB) must be(true)
      criterionsDB.contains(criterion2DB) must be(true)
      criterionsDB.contains(criterion3DB) must be(true)
    }

  }

  private def getCriterionsDataRowsFromTrial(trialId: Int): List[(Int, Int, Int, String, String, String, Option[Int])] = {
    database withSession {
      Query(Criterions).list.filter(c => c._3 == trialId)
    }
  }

}
