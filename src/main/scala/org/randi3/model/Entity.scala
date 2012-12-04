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

  def checkID(id: Int): ValidationNEL[String, Int] = {
    if (id == Int.MinValue || id >= 0) Success(id)
    else Failure("id not >= 0  or Int.MinValue").liftFailNel
  }

  def checkVersion(version: Int): ValidationNEL[String, Int] = {
    if (version >= 0) Success(version)
    else Failure("version < 0").liftFailNel
  }

  def checkNotNull[A](element: A): ValidationNEL[String, A] = {
    if (element != null) Success(element)
    else Failure("Element: " + element + " is null").liftFailNel
  }

  def checkIntMin(element: Int, min: Int): ValidationNEL[String, Int] = {
    if (element < min) Failure("Element must be >" + min).liftFailNel
    else element.success
  }

  def checkStringBetween(string: String, min: Int, max: Int): ValidationNEL[String, String] = {
    if (string == null) Failure("Text is not set").liftFailNel
    else if (string.size < min) Failure("Text length must be " + min + " minimal").liftFailNel
    else if (string.size > max) Failure("Text length must be " + max + " maximal").liftFailNel
    else string.success
  }

  def checkPassword(password: String): ValidationNEL[String, String] = {
    //SHA 512 Hash
    if (password.size == 128) password.success
    //Other passwords
    else if (password.size < 128) {
      //TODO check
      password.success
    } else Failure("password size have to be < 128").liftFailNel
  }

  def checkStringLength(string: String, length: Int): ValidationNEL[String, String] = checkStringBetween(string, length, length)

  def checkStartEnd(start: LocalDate, end: LocalDate): ValidationNEL[String, (LocalDate, LocalDate)] = {
    if (start == null) Failure("start date not set").liftFailNel
    else if (end == null) Failure("end date not set").liftFailNel
    else if (start.toDate.after(end.toDate)) Failure("end date before start date").liftFailNel
    else Success((start, end))
  }

  def checkListContainsMin[A](list: List[A], min: Int): ValidationNEL[String, List[A]] = {
    if (list != null && list.size >= min) Success(list)
    else Failure("List not set or or size smaller than " + min).liftFailNel
  }

  def checkListContainsExact[A](list: List[A], size: Int): ValidationNEL[String, List[A]] = {
    if (list != null && list.size == size) Success(list)
    else Failure("List not set or size not excatly " + size).liftFailNel
  }

  def checkAllElementsDefined[A](list: List[Option[A]]): ValidationNEL[String, List[Option[A]]] = {
    if (list != null && list.size == 1 && list.head.isDefined) Success(list)
    else if (list != null && list.size > 1 && list.map(element => element.isDefined).reduce((acc, el) => acc && el)) Success(list)
    else Failure("List not set or not all elements are defined").liftFailNel
  }

  def checkAtLeastOneElementIsDefined[A](list: List[Option[A]]): ValidationNEL[String, List[Option[A]]] = {
    if (list != null && list.map(element => element.isDefined).reduce((acc, el) => acc || el)) Success(list)
    else Failure("List not set or all elements are not defined").liftFailNel
  }

  def checkAll(checks: ValidationNEL[String, Any]*): ValidationNEL[String, Any] = {
    checks.reduce((acc, check) => (acc <|*|> check))
  }


}