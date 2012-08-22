package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.specs.runner.JUnitSuiteRunner

@RunWith(classOf[JUnitSuiteRunner])
class RandomizationPluginManagerSpec extends Spec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._

  describe("The RandomizationPluginManager getPluginName method") {

    it("should be able to return the CompleteRandomizationMethodName") {
      //println(randomizationPluginManager.getPluginNames)
    }

  }
}
