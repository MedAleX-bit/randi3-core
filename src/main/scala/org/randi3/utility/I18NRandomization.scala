package org.randi3.utility

import java.util.Locale


class I18NRandomization(properties: Map[Locale, Map[String, String]], securityUtility: AbstractSecurityUtil){


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
