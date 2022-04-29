package com.github.andyglow.jsonschema

import com.github.andyglow.json.{ ToValue, Value }
import com.github.andyglow.json.Value._
import json.Schema
import json.schema.Version
import zio.json.ast.{ Json => ZJson }
import zio.json.JsonEncoder

object AsZio {

  def apply[T](value: T): ZJson = value match {
    case `null`  => ZJson.Null
    case `true`  => ZJson.Bool(true)
    case `false` => ZJson.Bool(false)
    case num(x)  => ZJson.Num(x)
    case str(x)  => ZJson.Str(x)
    case arr(x)  =>
      val arr = x map AsZio.apply
      ZJson.Arr(arr.toSeq: _*)
    case obj(x)  =>
      val map = x.toMap.mapValues(AsZio.apply)
      ZJson.Obj(map.toSeq: _*)
  }

  implicit class ZioSchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asZio[V <: Version](v: V)(implicit asValue: AsValueBuilder[V]): ZJson = AsZio(
      AsValue.schema(x, v)
    )
  }

  implicit def toValue[T](implicit w: JsonEncoder[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js: Either[String,ZJson] = w.toJsonAST(x)
      def translate(js: ZJson): Value = js match {
        case ZJson.Bool(true) => `true`
        case ZJson.Bool(false) => `false`
        case ZJson.Null => `null`
        case ZJson.Num(bigDecimal) => num(bigDecimal)
        case ZJson.Str(string) => str(string)
        case ZJson.Arr(elementsChunk) => arr(elementsChunk.toArray.map(translate))
        case ZJson.Obj(fieldsChunk) =>
          val x: Map[String, ZJson] = fieldsChunk.toMap[String, ZJson]
          val y: Map[String, Value] = x.mapValues(translate)
          new obj(y)
      }

      // TODO: how to handle an error here?
      js.fold(msg => throw new Exception(msg), translate)
    }
  }

}
