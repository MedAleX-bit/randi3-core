package org.randi3.dao

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.specs.runner.JUnitSuiteRunner

@RunWith(classOf[JUnitSuiteRunner])
class RandomizationMethodSpec extends Spec with MustMatchers with ShouldMatchers {


  describe("The RandomizationMethodSpec create method") {

    it("should be able to create a complete randomization method") {

      /*   
   val allRandomizationMethodsCount =  database withSession {Query(RandomizationMethods).list.size}
      
      
      val id = randomizationMethodDao.create(new CompleteRandomization(Int.MinValue, 0, new MersenneTwister(1)), 1)
      
      database withSession {
        val allRandomizationMethods = Query(RandomizationMethods).list
        allRandomizationMethods.size must be(allRandomizationMethodsCount + 1)
        
        val randomizationMethodDB = allRandomizationMethods.filter(rm => rm._1.get == id).first
        randomizationMethodDB._2 must be(1)
        randomizationMethodDB._3.get.getClass().getSimpleName() must be("JdbcBlob")
        randomizationMethodDB._4 must be(classOf[CompleteRandomization].getName)
      }
       */
    }

  }
  /*
    describe("The RandomizationMethodSpec get method") {

      it("should be able to get a complete randomization method") {
        val completeMethod = new CompleteRandomization(Int.MinValue, 0, new MersenneTwister(1))
        val id = randomizationMethodDao.create(completeMethod, 1)
        val methodDB = randomizationMethodDao.get(id)
        methodDB.id must be(id)
        methodDB.getClass().getSimpleName() must be("CompleteRandomization")
        methodDB.random.nextInt must be(completeMethod.random.nextInt)
      }

    }

    describe("The RandomizationMethodSpec update method") {

      it("should be able to update a complete randomization method") {
        val completeMethod = new CompleteRandomization(Int.MinValue, 0, new MersenneTwister(1))
        val methodDB = randomizationMethodDao.get(randomizationMethodDao.create(completeMethod, 1))

        completeMethod.random.nextInt() must be(methodDB.random.nextInt())

        randomizationMethodDao.update(methodDB)
        val methodDB_Update = randomizationMethodDao.get(methodDB.id)
        completeMethod.random.nextInt() must be(methodDB_Update.random.nextInt())
      }

    }
  */
}
