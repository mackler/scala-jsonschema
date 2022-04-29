package com.github.andyglow.jsonschema

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.model.UserProfile
import json.schema.Version.Draft04
import org.scalactic.Equality
import zio.json._
import zio.json.ast.Json
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class AsZioSpec extends AnyPropSpec {
  import AsZioSpec._
  import UserProfileJson._

  private val examples = Table(
    ("json", "ZioJson"),
    (`null`, Json.Null),
    (`true`, Json.Bool(true)),
    (`false`, Json.Bool(false)),
    (str("foo"), Json.Str("foo")),
    (num(4), Json.Num(4)),
    (num(4.78), Json.Num(4.78)),
    (arr(1, 2, 3), Json.Arr(Json.Num(1), Json.Num(2), Json.Num(3))),
    (obj("foo" -> "foo", "bar" -> 15),
    Json.Obj("foo" -> Json.Str("foo"), "bar" -> Json.Num(15)))
  )

  property("Check that AsZio translates internal representation of json to Zio") {
    forAll(examples) { (internal, zio) => AsZio(internal) shouldEqual zio }
  }

  property("AsZio escapes") {
    val zobj: Json = AsZio(obj(""""quoted-key"""" -> "\n\t'val\""))
    zobj.toJson shouldBe """{"\"quoted-key\"":"\n\t'val\""}"""
  }

    property("Check Schema.asZio") {
    import AsZio._

    _root_.json.Json.schema[UserProfile].asZio(Draft04()) shouldEqual Json.Obj(
      f"$$schema"            -> Json.Str("http://json-schema.org/draft-04/schema#"),
      "type"                 -> Json.Str("object"),
      "additionalProperties" -> Json.Bool(false),
      "properties" -> Json.Obj(
        "firstName"   -> Json.Obj("type" -> Json.Str("string")),
        "middleName"  -> Json.Obj("type" -> Json.Str("string")),
        "lastName"    -> Json.Obj("type" -> Json.Str("string")),
        "age"         -> Json.Obj("type" -> Json.Str("integer")),
        "lastLoginMs" -> Json.Obj("type" -> Json.Str("number")),
        "role" -> Json.Obj(
          "type"    -> Json.Str("string"),
          "default" -> Json.Str("e-user"),
          "enum" -> Json.Arr(
            Json.Str("e-admin"),
            Json.Str("e-manager"),
            Json.Str("e-user")
          )
        ),
        "active" -> Json.Obj(
          "type"    -> Json.Str("string"),
          "default" -> Json.Str("On"),
          "enum" -> Json.Arr(Json.Str("On"), Json.Str("Off"), Json.Str("Suspended"))
        ),
        "enabledFeatures" -> Json.Obj(
          "type"        -> Json.Str("array"),
          "uniqueItems" -> Json.Bool(true),
          "default" -> Json.Arr(Json.Str("feature-0-name"), Json.Str("feature-1-name")),
          "items" -> Json.Obj(
            "type" -> Json.Str("string"),
            "enum" -> Json.Arr(
              Json.Str("feature-0-name"),
              Json.Str("feature-1-name"),
              Json.Str("feature-2-name")
            )
          )
        ),
        "credentials" -> Json.Obj(
          "type"                 -> Json.Str("object"),
          "additionalProperties" -> Json.Bool(false),
          "required"             -> Json.Arr(Json.Str("login"), Json.Str("password")),
          "properties" -> Json.Obj(
            "login"       -> Json.Obj("type" -> Json.Str("string")),
            "password"    -> Json.Obj("type" -> Json.Str("string"))
          ),
          "default" -> Json.Obj(
            "login" -> Json.Str("anonymous"),
            "password" -> Json.Str("-")
          )
        ),
        "notes" -> Json.Obj(
          "type"                 -> Json.Str("object"),
          "additionalProperties" -> Json.Bool(false),
          "required"             -> Json.Arr(Json.Str("head"), Json.Str("tail")),
          "properties" -> Json.Obj(
            "head" -> Json.Obj("type" -> Json.Str("string")),
            "tail" -> Json.Obj(
              "type"  -> Json.Str("array"),
              "items" -> Json.Obj("type" -> Json.Str("string"))
            )
          ),
          "default" -> Json.Obj("head" -> Json.Str("initial note"), "tail" -> Json.Arr())
        )
      ),
      "required" -> Json.Arr(
        Json.Str("age"),
        Json.Str("lastName"),
        Json.Str("firstName")
      )
    )
  }

}


object AsZioSpec {

  implicit val jsValEq: Equality[Json] = new Equality[Json] {
    override def areEqual(a: Json, b: Any): Boolean = a match {
      case Json.Null => b == Json.Null
      case Json.Bool(true)    => b == Json.Bool(true)
      case Json.Bool(false)   => b == Json.Bool(false)
      case Json.Num(a) if b.isInstanceOf[Json.Num] => b.asInstanceOf[Json.Num].value == a
      case Json.Str(a) if b.isInstanceOf[Json.Str] => b.asInstanceOf[Json.Str].value == a
      case a: Json.Arr => jsArrEq.areEqual(a, b)
      case a: Json.Obj => jsObjEq.areEqual(a, b)
      case _ => false
    }
  }

  implicit val jsArrEq: Equality[Json.Arr] = new Equality[Json.Arr] {

    override def areEqual(a: Json.Arr, b: Any): Boolean = b match {
      case b: Json.Arr => {
        if (a.elements.length == b.elements.length) {
          a.elements forall { aa =>
            b.elements exists { bb =>
              jsValEq.areEqual(aa, bb)
            }
          }
        } else false
      }
      case _ => false
    }
  }

  implicit val jsObjEq: Equality[Json.Obj] = new Equality[Json.Obj] {

    override def areEqual(a: Json.Obj, b: Any): Boolean = b match {
      case Json.Obj(bFields) =>
        val keys = a.fields.toMap.keys ++ bFields.toMap.keys
        keys.foldLeft(true) {
          case (true, k) =>
            val r = for {
              a <- a.fields.toMap.get(k)
              b <- bFields.toMap.get(k)
            } yield jsValEq.areEqual(a, b)
            r getOrElse false

          case (false, _) => false
        }
      case _ => false
    }

  }

}
