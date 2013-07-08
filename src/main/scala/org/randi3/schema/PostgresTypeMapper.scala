package org.randi3.schema

import scala.slick.lifted.{TypeMapperDelegate, BaseTypeMapper}
import scala.slick.driver.BasicProfile
import scala.slick.session.{PositionedResult, PositionedParameters}
import java.sql.SQLException


object PostgresByteArrayTypeMapper extends
BaseTypeMapper[Array[Byte]] with TypeMapperDelegate[Array[Byte]] {
  def apply(p: BasicProfile) = this
  val zero = new Array[Byte](0)
  val sqlType = java.sql.Types.BLOB
  override val sqlTypeName = "BYTEA"
  def setValue(v: Array[Byte], p: PositionedParameters) {
    p.pos += 1
    p.ps.setBytes(p.pos, v)
  }
  def setOption(v: Option[Array[Byte]], p: PositionedParameters) {
    p.pos += 1
    if(v eq None) p.ps.setBytes(p.pos, null) else p.ps.setBytes(p.pos, v.get)
  }
  def nextValue(r: PositionedResult) = {
    r.nextBytes()
  }
  def updateValue(v: Array[Byte], r: PositionedResult) {
    r.updateBytes(v)
  }

  override def valueToSQLLiteral(value: Array[Byte]) =
    throw new SQLException("Cannot convert BYTEA to literal")
}