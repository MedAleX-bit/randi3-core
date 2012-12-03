package org.randi3.dao

import java.sql.Date

import org.randi3.model.criterion._
import org.scalaquery.session.Database.threadLocalSession
import org.randi3.model.criterion.constraint._
import scalaz._
import org.joda.time.LocalDate
import collection.mutable.{HashMap, ListBuffer}
import org.randi3.utility._
import org.scalaquery.ql.Parameters

trait CriterionDaoComponent {

  this: DaoComponent with
    UtilityDBComponent =>

  val criterionDao: CriterionDao

  class CriterionDao {

    import driver.Implicit._
    import schema._
    import utilityDB._

    def queryCriterionFromTrialAndCriterionName(trialId: Int, criterionName: String) = for {
      criterion <- Criterions if criterion.name === criterionName && criterion.trialId === trialId
    } yield criterion.id ~ criterion.version ~ criterion.trialId ~ criterion.name ~ criterion.description ~ criterion.criterionType

    val queryCriterionsFromTrialId = for {
      trialId <- Parameters[Int]
      criterion <- Criterions if criterion.trialId === trialId
    } yield criterion.id ~ criterion.version ~ criterion.trialId ~ criterion.name ~ criterion.description ~ criterion.criterionType ~ criterion.inclusionConstraintId

    val queryCriterionFromId = for {
      id <- Parameters[Int]
      criterion <- Criterions if criterion.id === id
    } yield criterion.id ~ criterion.version ~ criterion.trialId ~ criterion.name ~ criterion.description ~ criterion.criterionType ~ criterion.inclusionConstraintId

    val queryOrdinalCriterionValuesFromId = for {
      id <- Parameters[Int]
      ordinalValue <- OrdinalCriterionValues if ordinalValue.criterionId === id
    } yield ordinalValue.value

    val queryConstraintIDFromUUID = for {
      uuid <- Parameters[String]
      constraint <- Constraints if constraint.uuid === uuid
    } yield constraint.id

    def queryConstraintFromIds(ids: List[Int]) = for {
      constraint <- Constraints if constraint.id inSetBind ids
    } yield constraint.id ~ constraint.version ~ constraint.constraintType ~ constraint.expectedValueFreeText ~ constraint.firstDate ~ constraint.secondDate ~ constraint.firstDouble ~ constraint.secondDouble ~ constraint.firstInteger ~ constraint.secondInteger

    val queryOrdinalConstraintValuesFromConstraintId = for {
      id <- Parameters[Int]
      ordinalConstraintValues <- OrdinalConstraintValues if ordinalConstraintValues.constraintId === id
    } yield ordinalConstraintValues.value

    val queryAllConstraintIdFromStrataWithCriterionId = for {
      id <- Parameters[Int]
      stratum <- Strata if stratum.criterionId === id
    } yield stratum.constraintId

    val queryStrataWithCriterionId = for {
      id <- Parameters[Int]
      stratum <- Strata if stratum.criterionId === id
    } yield stratum.*


    val queryStagesFromTrial = for {
      id <- Parameters[Int]
      trialStage <- TrialStages if trialStage.trialId === id
    } yield trialStage.stageName ~ trialStage.criterionId

    def create[T](criterion: Criterion[T, Constraint[T]], trialId: Int): Validation[String, Int] = {
      onDB {
        val inclusionCriterion = {
          threadLocalSession withTransaction {
            if (criterion.inclusionConstraint.isDefined) Some(createConstraint(criterion.inclusionConstraint.get).either match {
              case Left(x) => return Failure(x)
              case Right(x) => x
            })
            else None
          }
        }

        threadLocalSession withTransaction {
          Criterions.noId insert(criterion.version, trialId, criterion.name, criterion.description, criterion.getClass.getName, inclusionCriterion)
        }
        val id = getId(trialId, criterion.name).either match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }

        if (criterion.isInstanceOf[OrdinalCriterion]) {
          createOrdinalCriterion(criterion.asInstanceOf[OrdinalCriterion], id)
        }

        //create Strata
        val strataIds = new ListBuffer[Int]()
        //create constraint for the strata
        threadLocalSession withTransaction {
          criterion.strata.foreach(stratum => strataIds.append(createConstraint(stratum).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }))
        }
        //create strata table entries
        threadLocalSession withTransaction {
          strataIds.foreach(strataId => Strata.noId insert(0, id, strataId))
        }

        Success(id)
      }
    }

    private def getId(trialId: Int, criterionName: String): Validation[String, Int] = {
      //TODO change
      for (c <- queryCriterionFromTrialAndCriterionName(trialId, criterionName)) return Success(c._1)
      Failure("Criterion not found")
    }

    private def createOrdinalCriterion(ordinalCriterion: OrdinalCriterion, criterionId: Int) {
      threadLocalSession withTransaction {
        ordinalCriterion.values.foreach(value => OrdinalCriterionValues.noId insert(criterionId, value))
      }
    }

    private def createConstraint[T](constraint: Constraint[T]): Validation[String, Int] = {
      val uuid = java.util.UUID.randomUUID().toString
      val clazz = constraint.getClass

      if (clazz == classOf[DateConstraint]) {
        val constr = constraint.asInstanceOf[DateConstraint]
        val firstDate = if (constr.firstValue.isDefined) Some(new Date(constr.firstValue.get.toDate.getTime)) else None
        val secondDate = if (constr.secondValue.isDefined) Some(new Date(constr.secondValue.get.toDate.getTime)) else None
        Constraints.noId insert(constraint.version, constraint.getClass.getName, None, firstDate, secondDate, None, None, None, None, uuid)
        getConstraintIdFromUUID(uuid)

      } else if (clazz == classOf[DoubleConstraint]) {
        val constr = constraint.asInstanceOf[DoubleConstraint]
        Constraints.noId insert(constraint.version, constraint.getClass.getName, None, None, None, constr.firstValue, constr.secondValue, None, None, uuid)
        getConstraintIdFromUUID(uuid)

      } else if (clazz == classOf[IntegerConstraint]) {
        val constr = constraint.asInstanceOf[IntegerConstraint]
        Constraints.noId insert(constraint.version, constraint.getClass.getName, None, None, None, None, None, constr.firstValue, constr.secondValue, uuid)
        getConstraintIdFromUUID(uuid)

      } else if (clazz == classOf[FreeTextConstraintExact]) {
        val constr = constraint.asInstanceOf[FreeTextConstraintExact]
        Constraints.noId insert(constraint.version, constraint.getClass.getName, Some(constr.expectedValue), None, None, None, None, None, None, uuid)
        getConstraintIdFromUUID(uuid)

      } else if (clazz == classOf[FreeTextConstraintNotEmpty]) {
      Constraints.noId insert(constraint.version, constraint.getClass.getName, None, None, None, None, None, None, None, uuid)
      getConstraintIdFromUUID(uuid)

    }   else if (clazz == classOf[OrdinalConstraint]) {
        val constr = constraint.asInstanceOf[OrdinalConstraint]
        Constraints.noId insert(constraint.version, constraint.getClass.getName, None, None, None, None, None, None, None, uuid)
        val id = getConstraintIdFromUUID(uuid).either match {
          case Left(x) => return Failure(x)
          case Right(x) => x
        }
        createOrdinalConstraintValue(id, constr.expectedValues)
        Success(id)

      } else Failure("unknown constraint typ: " + constraint.getClass.getName)
    }

    private def getConstraintIdFromUUID(uuid: String): Validation[String, Int] = {
      val resultList = queryConstraintIDFromUUID(uuid).list
      if (resultList.size == 1) Success(resultList(0))
      else if (resultList.isEmpty) Failure("uuid not found")
      else Failure("more than one uuid found")
    }

    private def createOrdinalConstraintValue(constraintId: Int, constraintValues: Set[String]) {
      constraintValues.foreach(value => OrdinalConstraintValues.noId insert(0, constraintId, value))
    }

    def getCriterions(trialId: Int): Validation[String, List[Criterion[Any, Constraint[Any]]]] = {
      onDB {
        val resultList = new ListBuffer[Criterion[Any, Constraint[Any]]]()
        for (c <- queryCriterionsFromTrialId(trialId)) {
          val criterion = generateCriterionFromDatabaseRow(c).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }
          resultList += criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]
        }
        Success(resultList.toList)
      }
    }

    def getStages(trialId: Int): Validation[String, Map[String, List[Criterion[Any, Constraint[Any]]]]] = {
      onDB {

        val resultList = queryStagesFromTrial(trialId).list
        val uniqueStages = resultList.map(element => element._1).toSet
        val stages = new HashMap[String, List[Criterion[Any, Constraint[Any]]]]()
        uniqueStages.foreach {
          stageName =>
            val criterions = new ListBuffer[Criterion[Any, Constraint[Any]]]()
            val stageCriterions = resultList.filter(stage => stage._1 == stageName).map(element => element._2)
            stageCriterions.foreach {
              critId =>
                val row = queryCriterionFromId(critId).list.head
                val criterion = generateCriterionFromDatabaseRow(row).either match {
                  case Left(x) => return Failure(x)
                  case Right(x) => x
                }
                criterions += criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]
            }
            stages.put(stageName, criterions.toList)
        }
        Success(stages.toMap.asInstanceOf[Map[String, List[Criterion[Any, Constraint[Any]]]]])
      }
    }

    def get[T](criterionId: Int): Validation[String, Option[Criterion[T, Constraint[T]]]] = {
      onDB {
        val criterionResultList = queryCriterionFromId(criterionId).list
        if (criterionResultList.isEmpty) Success(None)
        else if (criterionResultList.size > 1) Failure("Duplicated criterion id found")
        else generateCriterionFromDatabaseRow(criterionResultList(0)).either match {
          case Left(x) => Failure(x)
          case Right(x) => Success(Some(x.asInstanceOf[Criterion[T, Constraint[T]]]))
        }
      }
    }


    def update[T](criterion: Criterion[T, Constraint[T]]): Validation[String, Boolean] = {
      onDB {
        val critDb = get(criterion.id).toOption.get.get

        val inclusionCriterion = {
          threadLocalSession withTransaction {
            if (criterion.inclusionConstraint.isDefined) Some(createConstraint(criterion.inclusionConstraint.get).either match {
              case Left(x) => return Failure(x)
              case Right(x) => x
            })
            else None
          }
        }


        threadLocalSession withTransaction {
          queryCriterionFromId(criterion.id).mutate {
            r => r.row = r.row.copy(_2 = criterion.version, _4 = criterion.name, _5 = criterion.description, _7 = inclusionCriterion)
          }
        }

        threadLocalSession withTransaction {

          if (critDb.inclusionConstraint.isDefined) {
            queryConstraintFromIds(List(critDb.inclusionConstraint.get.id)).mutate {
              r => r.delete()
            }
          }

        }
        val oldConstraintFromStrata = queryAllConstraintIdFromStrataWithCriterionId(critDb.id).list()

        threadLocalSession withTransaction {
          //remove strata constraints
          queryConstraintFromIds(oldConstraintFromStrata).mutate(
            r => r.delete()
          )

        }

        threadLocalSession withTransaction {
          //remove strata
          queryStrataWithCriterionId(critDb.id).mutate(
            r => r.delete()
          )

        }


        //create Strata
        val strataIds = new ListBuffer[Int]()
        //create constraint for the strata
        threadLocalSession withTransaction {
          criterion.strata.foreach(stratum => strataIds.append(createConstraint(stratum).either match {
            case Left(x) => return Failure(x)
            case Right(x) => x
          }))
        }
        //create strata table entries
        threadLocalSession withTransaction {
          strataIds.foreach(strataId => Strata.noId insert(0, critDb.id, strataId))
        }

        Success(true)
      }
    }

    def delete(criterionId: Int): Validation[String, Boolean] = {
      onDB {
        val critDb = get(criterionId).toOption.get.get

        threadLocalSession withTransaction {
          queryCriterionFromId(criterionId).mutate {
            r => r.row = r.row.copy(_7 = None)
          }
        }


        threadLocalSession withTransaction {
          if (critDb.inclusionConstraint.isDefined) {
            queryConstraintFromIds(List(critDb.inclusionConstraint.get.id)).mutate {
              r => r.delete()
            }
          }
        }


        val oldConstraintFromStrata = queryAllConstraintIdFromStrataWithCriterionId(critDb.id).list()

        threadLocalSession withTransaction {
          //remove strata constraints
          queryConstraintFromIds(oldConstraintFromStrata).mutate(
            r => r.delete()
          )

        }

        threadLocalSession withTransaction {
          //remove strata
          queryStrataWithCriterionId(critDb.id).mutate(
            r => r.delete()
          )

        }

        threadLocalSession withTransaction {
          queryCriterionFromId(criterionId).mutate {
            r => r.delete()
          }

        }

        Success(true)
      }
    }

    private def generateCriterionFromDatabaseRow[T](dataRow: (Int, Int, Int, java.lang.String, String, String, Option[Int])): Validation[String, Criterion[T, Constraint[T]]] = {
      val inclusionConstraint = getInclusionConstraint(dataRow._7).either match {
        case Left(x) => return Failure(x)
        case Right(x) => x
      }
      val strata = getStrata(dataRow._1).either match {
        case Left(x) => return Failure(x)
        case Right(x) => x
      }

      if (dataRow._6 == classOf[FreeTextCriterion].getName) {
        FreeTextCriterion(id = dataRow._1, version = dataRow._2, name = dataRow._4, description = dataRow._5,
          inclusionConstraint = inclusionConstraint,
          strata = strata).either match {
          case Left(x) => return Failure("Database entry corrupt" + x.toString())
          case Right(x) => return Success(x.asInstanceOf[Criterion[T, Constraint[T]]])
        }
      } else if (dataRow._6 == classOf[DateCriterion].getName) {
        DateCriterion(id = dataRow._1, version = dataRow._2, name = dataRow._4, description = dataRow._5,
          inclusionConstraint = inclusionConstraint,
          strata = strata).either match {
          case Left(x) => return Failure("Database entry corrupt" + x.toString())
          case Right(x) => return Success(x.asInstanceOf[Criterion[T, Constraint[T]]])
        }

      } else if (dataRow._6 == classOf[IntegerCriterion].getName) {
        IntegerCriterion(id = dataRow._1, version = dataRow._2, name = dataRow._4, description = dataRow._5,
          inclusionConstraint = inclusionConstraint,
          strata = strata).either match {
          case Left(x) => return Failure("Database entry corrupt" + x.toString())
          case Right(x) => return Success(x.asInstanceOf[Criterion[T, Constraint[T]]])
        }
      } else if (dataRow._6 == classOf[DoubleCriterion].getName) {
        DoubleCriterion(id = dataRow._1, version = dataRow._2, name = dataRow._4, description = dataRow._5,
          inclusionConstraint = inclusionConstraint,
          strata = strata).either match {
          case Left(x) => return Failure("Database entry corrupt" + x.toString())
          case Right(x) => return Success(x.asInstanceOf[Criterion[T, Constraint[T]]])
        }
      } else if (dataRow._6 == classOf[OrdinalCriterion].getName) {
        OrdinalCriterion(id = dataRow._1, version = dataRow._2, name = dataRow._4, description = dataRow._5, values = generateOrdinalCriterionValues(dataRow._1),
          inclusionConstraint = inclusionConstraint,
          strata = strata).either match {
          case Left(x) => return Failure("Database entry corrupt" + x.toString())
          case Right(x) => return Success(x.asInstanceOf[Criterion[T, Constraint[T]]])
        }

      } else return Failure("criterion type not found: " + dataRow._6)

    }

    private def generateOrdinalCriterionValues(criterionId: Int): Set[String] = {
      val resultSet = new scala.collection.mutable.HashSet[String]()
      for (value <- queryOrdinalCriterionValuesFromId(criterionId)) {
        resultSet.add(value)
      }
      resultSet.toSet
    }

    private def getInclusionConstraint[T](constraintIdOp: Option[Int]): Validation[String, Option[T]] = {
      val constraintId = constraintIdOp.getOrElse(return Success(None))

      val resultList = getConstraints(List(constraintId)).either match {
        case Left(x) => return Failure(x)
        case Right(x) => x
      }

      if (resultList.size > 1) Failure("only one inclusion constraint can be defined")

      else if (resultList.size == 0) Failure("inclusion constraint not found")

      else Success(Some(resultList(0)))
    }

    private def getStrata[T](criterionId: Int): Validation[String, List[T]] = {
      getConstraints(queryAllConstraintIdFromStrataWithCriterionId(criterionId).list)
    }

    private def getConstraints[T](constraintIds: List[Int]): Validation[String, List[T]] = {
      val results = new ListBuffer[T]()

      val resultList = queryConstraintFromIds(constraintIds).list

      resultList.foreach {
        constraint =>
          results.append((
            if (constraint._3 == classOf[FreeTextConstraintExact].getName) {
              FreeTextConstraintExact(id = constraint._1, version = constraint._2, configurations = List(constraint._4))
            } else if (constraint._3 == classOf[FreeTextConstraintNotEmpty].getName) {
              FreeTextConstraintNotEmpty(id = constraint._1, version = constraint._2)
            } else if (constraint._3 == classOf[DateConstraint].getName) {
              val firstDate = if (constraint._5.isDefined) Some(new LocalDate(constraint._5.get.getTime)) else None
              val secondDate = if (constraint._6.isDefined) Some(new LocalDate(constraint._6.get.getTime)) else None
              DateConstraint(id = constraint._1, version = constraint._2, configurations = List(firstDate, secondDate))
            } else if (constraint._3 == classOf[DoubleConstraint].getName) {
              DoubleConstraint(id = constraint._1, version = constraint._2, configurations = List(constraint._7, constraint._8))
            } else if (constraint._3 == classOf[IntegerConstraint].getName) {
              IntegerConstraint(id = constraint._1, version = constraint._2, configurations = List(constraint._9, constraint._10))
            } else if (constraint._3 == classOf[OrdinalConstraint].getName) {
              val ordinalConstraintValues = queryOrdinalConstraintValuesFromConstraintId(constraint._1).list.map(entry => Some(entry))
              OrdinalConstraint(id = constraint._1, version = constraint._2, configurations = ordinalConstraintValues)
            } else return Failure("constraint type not found: " + constraint._3)
            ).either match {
            case Left(x) => return Failure("Database entry corrupt " + x.toString())
            case Right(actConstraint) => actConstraint.asInstanceOf[T]
          })
      }
      Success(results.toList)
    }


  }

}
