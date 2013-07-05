package org.randi3.randomization

import org.clapper.classutil.ClassFinder
import java.io.File
import org.randi3.dao.DaoComponent
import java.net.URLClassLoader
import org.randi3.configuration.{ConfigurationServiceComponent, ConfigurationValues}
import collection.mutable
import org.randi3.utility.SecurityComponent


trait RandomizationPluginManagerComponent {

  this: DaoComponent with
    ConfigurationServiceComponent with
    SecurityComponent =>

  val randomizationPluginManager: RandomizationPluginManager

  class RandomizationPluginManager {

    private val randomizationMethodMap = mutable.HashMap[String, RandomizationMethodPlugin]()

    def init() {

      randomizationMethodMap.clear()

      val path = configurationService.getConfigurationEntry(ConfigurationValues.PLUGIN_PATH.toString).toOption.get

      if (new File(path).exists()) {
        val classpath = new File(path).listFiles.toList.filter(file => file.isFile & file.canRead)

       if (classpath != null && !classpath.isEmpty) {
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
              val plugin = constructor.newInstance(database, driver, securityUtility).asInstanceOf[RandomizationMethodPlugin]
              randomizationMethodMap.put(plugin.name, plugin)
          }
        }
      }
    }

    def getPlugin(name: String): Option[RandomizationMethodPlugin] = {
      if (randomizationMethodMap.isEmpty) init()
      randomizationMethodMap.get(name)
    }

    def getPluginNames: Set[String] = {
      if (randomizationMethodMap.isEmpty) init()
      randomizationMethodMap.keySet.toSet
    }


    def getPluginNamesWithI18N: Set[(String, String)] = {
      if (randomizationMethodMap.isEmpty) init()
      randomizationMethodMap.map(entry => (entry._1, entry._2.i18nName)).toSet
    }

    def getPluginForMethod(method: RandomizationMethod): Option[RandomizationMethodPlugin] = {
      getPlugin(method.getClass.getName)
    }

  }

}
