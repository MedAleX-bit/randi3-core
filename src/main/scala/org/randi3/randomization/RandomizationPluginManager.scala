package org.randi3.randomization

import org.clapper.classutil.ClassFinder
import java.io.File
import org.randi3.dao.DaoComponent
import java.net.URLClassLoader
import org.randi3.configuration.{ConfigurationValues, ConfigurationService}

trait RandomizationPluginManagerComponent extends DaoComponent {

  val randomizationPluginManager: RandomizationPluginManager

  class RandomizationPluginManager {

    val configurationService = new ConfigurationService

    var randomizationMethodMap = Map[String, RandomizationMethodPlugin]()

    def init() {

      //TODO check
      val path = configurationService.getConfigurationEntry(ConfigurationValues.PLUGIN_PATH.toString).toOption.get

      val classpath = new File(path).listFiles

      val urls = (classpath.map(file => file.toURI.toURL)).toArray

      val classloader = new URLClassLoader(urls, this.getClass.getClassLoader)

      val finder = ClassFinder(classpath)
      val classes = finder.getClasses()
      val classMap = ClassFinder.classInfoMap(classes)

      val plugins = classMap.values.filter(classInfo => classInfo.superClassName == "org.randi3.randomization.RandomizationMethodPlugin")

      plugins.foreach {
        pluginString =>
          val clazz = classloader.loadClass(pluginString.name)
          val constructor = clazz.getConstructors.head
          val plugin = constructor.newInstance(database, driver).asInstanceOf[RandomizationMethodPlugin]
          //TODO DB initialization
          randomizationMethodMap += (plugin.name -> plugin)
      }
    }

    def getPlugin(name: String): Option[RandomizationMethodPlugin] = {
      if (randomizationMethodMap.isEmpty) init()
      randomizationMethodMap.get(name)
    }

    def getPluginNames: Set[String] = {
      if (randomizationMethodMap.isEmpty) init()
      randomizationMethodMap.keySet
    }

  }

}
