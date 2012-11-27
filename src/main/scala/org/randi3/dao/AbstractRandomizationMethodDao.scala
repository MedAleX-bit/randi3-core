package org.randi3.dao


import org.randi3.schema.DatabaseSchema._
import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.ExtendedProfile
import javax.sql.rowset.serial.SerialBlob
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.sql.Blob
import org.apache.commons.math3.random.RandomGenerator
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import scalaz._

import org.scalaquery.ql.Parameters

abstract class AbstractRandomizationMethodDao(database: Database, driver: ExtendedProfile){

  import driver.Implicit._
  val schema =  org.randi3.schema.DatabaseSchema.schema(driver)
  import schema._

  val queryRandomizationMethodFromId = for {
    id <- Parameters[Int]
    rm <- RandomizationMethods if rm.id is id
  } yield rm.id ~ rm.randomGenerator ~ rm.randomizationType

  val queryRandomizationMethodFromTrialId = for {
    trialId <- Parameters[Int]
    rm <- RandomizationMethods if rm.trialId is trialId
  } yield rm.id ~ rm.trialId ~ rm.randomGenerator ~ rm.randomizationType

  def getId(trialId: Int): Validation[String, Int] = {
    for (rm <- queryRandomizationMethodFromTrialId(trialId)) return Success(rm._1.get)
    Failure("Randomization method not found")
  }

  protected def generateBlob(ob: Any): Option[Blob] = {
    val bos: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos: ObjectOutputStream = new ObjectOutputStream(bos)
    oos.writeObject(ob)
    oos.flush()
    oos.close()
    bos.close()

    Some(new SerialBlob(bos.toByteArray))
  }

  protected def deserializeRandomGenerator(ob: Blob): RandomGenerator = {
    val bais = new ByteArrayInputStream(ob.getBytes(1, ob.length().toInt))
    val in = new ObjectInputStream(bais)
    in.readObject().asInstanceOf[RandomGenerator]
  }

}
