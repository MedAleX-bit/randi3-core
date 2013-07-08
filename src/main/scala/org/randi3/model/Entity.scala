package org.randi3.model

import scalaz._
import Scalaz._
import java.util.Date
import org.joda.time.LocalDate

abstract class Entity {

  val id: Int

  val version: Int

}


object Entity {

  val maxTextLength = 256

  def checkID(id: Int): ValidationNel[String, Int] = {
    if (id == Int.MinValue || id >= 0) Success(id)
    else Failure("id not >= 0  or Int.MinValue").toValidationNel
  }

  def checkVersion(version: Int): ValidationNel[String, Int] = {
    if (version >= 0) Success(version)
    else Failure("version < 0").toValidationNel
  }

  def checkNotNull[A](element: A): ValidationNel[String, A] = {
    if (element != null) Success(element)
    else Failure("Element: " + element + " is null").toValidationNel
  }

  def checkIntMin(element: Int, min: Int): ValidationNel[String, Int] = {
    if (element < min) Failure("Element must be >" + min).toValidationNel
    else element.success
  }

  def checkStringBetween(string: String, min: Int, max: Int): ValidationNel[String, String] = {
    if (string == null) Failure("Text is not set").toValidationNel
    else if (string.size < min) Failure("Text length must be " + min + " minimal").toValidationNel
    else if (string.size > max) Failure("Text length must be " + max + " maximal").toValidationNel
    else string.success
  }

  def checkPassword(password: String): ValidationNel[String, String] = {
    //SHA 512 Hash
    if (password.size == 128) password.success
    //Other passwords
    else if (password.size < 128) {
      //TODO check
      password.success
    } else Failure("password size have to be < 128").toValidationNel
  }

  def checkStringLength(string: String, length: Int): ValidationNel[String, String] = checkStringBetween(string, length, length)

  def checkStartEnd(start: LocalDate, end: LocalDate): ValidationNel[String, (LocalDate, LocalDate)] = {
    if (start == null) Failure("start date not set").toValidationNel
    else if (end == null) Failure("end date not set").toValidationNel
    else if (start.toDate.after(end.toDate)) Failure("end date before start date").toValidationNel
    else Success((start, end))
  }

  def checkListContainsMin[A](list: List[A], min: Int): ValidationNel[String, List[A]] = {
    if (list != null && list.size >= min) Success(list)
    else Failure("List not set or or size smaller than " + min).toValidationNel
  }

  def checkListContainsExact[A](list: List[A], size: Int): ValidationNel[String, List[A]] = {
    if (list != null && list.size == size) Success(list)
    else Failure("List not set or size not excatly " + size).toValidationNel
  }

  def checkAllElementsDefined[A](list: List[Option[A]]): ValidationNel[String, List[Option[A]]] = {
    if (list != null && list.size == 1 && list.head.isDefined) Success(list)
    else if (list != null && list.size > 1 && list.map(element => element.isDefined).reduce((acc, el) => acc && el)) Success(list)
    else Failure("List not set or not all elements are defined").toValidationNel
  }

  def checkAtLeastOneElementIsDefined[A](list: List[Option[A]]): ValidationNel[String, List[Option[A]]] = {
    if (list != null && list.map(element => element.isDefined).reduce((acc, el) => acc || el)) Success(list)
    else Failure("List not set or all elements are not defined").toValidationNel
  }

  def checkAll(checks: ValidationNel[String, Any]*): ValidationNel[String, Any] = {
    checks.reduce((acc, check) => (acc <|*|> check))
  }


}