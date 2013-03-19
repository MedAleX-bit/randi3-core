package org.randi3.utility

import java.util.Locale
import java.io.{InputStream, File}
import collection.mutable.{ListBuffer, HashMap}
import collection.mutable
import java.util

trait I18NComponent {

  this: SecurityComponent =>

  val i18n: I18N

  class I18N(properties: Map[Locale, Map[String, String]]) {

    import securityUtility._

    def text(label: String): String = {
      def default: String = {
        properties.get(Locale.ENGLISH).getOrElse(return label).
          get(label).getOrElse(return label)
      }

      if (currentUser.isDefined) {

        def language: String = {
          val languageLoc = Locale.getAvailableLocales.find(loc => loc.getCountry.isEmpty && loc.getLanguage == currentUser.get.locale.getLanguage).getOrElse(return default)
          properties.get(languageLoc).getOrElse(return label).
            get(label).getOrElse(return default)
        }

        val entries = properties.get(currentUser.get.locale).getOrElse(return language)
        entries.get(label).getOrElse(return language)

      } else default
    }

    def getAvailableLocales: List[Locale] = {
      properties.map(element => element._1).toList
    }


  }

  object I18N {

    def apply(): I18N = {

      new I18N(I18NHelper.getLocalizationMap("messages"))
    }


  }

}


object I18NHelper {


  def getLocalizationMap(resourceName: String, classLoader: ClassLoader):Map[Locale, Map[String, String]] ={

    val entries = new mutable.HashMap[Locale, Map[String, String]]()

    val stream = classLoader.getResourceAsStream(resourceName +".properties")

    entries.put(Locale.ENGLISH, getEntries(stream))

    Locale.getAvailableLocales.foreach(loc => {
      val language = if (loc.getCountry.isEmpty) loc.getLanguage else loc.getLanguage+ "_" + loc.getCountry
      val stream = classLoader.getResourceAsStream(resourceName+"_"+ language +".properties")
      if(stream != null){
        entries.put(loc, getEntries(stream))
      }
    })

    entries.toMap

  }


  def getLocalizationMap(resourceName: String):Map[Locale, Map[String, String]] ={

    getLocalizationMap(resourceName, getClass.getClassLoader)

  }

  private def getEntries(stream: InputStream): Map[String, String] = {
    val entryMap = new HashMap[String, String]()
    scala.io.Source.fromInputStream(stream).getLines().foreach(line => {
      val entry = line.split("=", 2)
      if (entry.size == 2) {
        entryMap.put(entry(0).trim, entry(1).trim)
      }
    })
    entryMap.toMap
  }
}
