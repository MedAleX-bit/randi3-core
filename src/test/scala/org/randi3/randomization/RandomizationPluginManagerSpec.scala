package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RandomizationPluginManagerSpec extends FunSpec with MustMatchers with ShouldMatchers {

   describe("The RandomizationPluginManager getPluginName method") {

    it("should be able to return the CompleteRandomizationMethodName") {
      //println(randomizationPluginManager.getPluginNames)
    }

  }
}
