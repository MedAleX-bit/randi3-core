package org.randi3.schema

import org.scalaquery.ql.{TypeMapperDelegate, BaseTypeMapper}
import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.SQueryException

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
    throw new SQueryException("Cannot convert BYTEA to literal")
}