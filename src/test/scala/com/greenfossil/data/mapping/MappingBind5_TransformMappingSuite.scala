/*
 * Copyright 2022 Greenfossil Pte Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.greenfossil.data.mapping

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.{LocalDate, LocalTime}
import scala.util.Try

class MappingBind5_TransformMappingSuite extends munit.FunSuite {

  import Mapping.*

  import java.time.*

  test("transform") {
    val f1 = number.name("f1")
      .transform[Int](_ * 2, _ / 2)
      .verifying("X must be 8", x => x == 8)

    //Bind Missing Value
    val boundMissingValueField = f1.bind()
    assertEquals(boundMissingValueField.typedValueOpt, None)
    assertEquals(boundMissingValueField.bindingValueOpt, None)
    assertEquals(boundMissingValueField.errors.map(_.message), Seq("error.required")) //emitted by number

    //Bind Empty Value
    val boundEmptyValueField = f1.bind("f1" -> "")
    assertEquals(boundEmptyValueField.typedValueOpt, None)
    assertEquals(boundEmptyValueField.bindingValueOpt, Some(""))
    assertEquals(boundEmptyValueField.errors.map(_.message), Seq("error.number")) //emitted by number

    val boundField = f1.bind("f1" -> "4")
    assertEquals(boundField.typedValueOpt, Some(8))
    assertEquals(boundField.bindingValueOpt, Some("4"))
    assert(boundField.errors.isEmpty)
  }

  test("fill form handles null") {
    val form = Mapping("name", text.transform(_.trim, _.trim))
    val name: String = null
    val filledForm = form.fill(name)
    assertEquals(filledForm.typedValueOpt, Some(null))
    assertEquals(filledForm.bindingValueOpt, None)
  }

  test("transform mapping from optional string to Seq[String]") {
    def basicFilterForm = Mapping(
      "filters", optional[String].transform[Seq[String]](
        opt =>
          opt.map(_.split(",").toSeq).getOrElse(Nil),
        l =>
          Option(l.mkString(", ")).filter(_.nonEmpty)
      )
    )

    val boundMissingValueForm = basicFilterForm.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, Option(Nil))

    val boundEmptyValueForm = basicFilterForm.bind("filters" -> "")
    assertEquals(boundEmptyValueForm.typedValueOpt, Option(Nil))

    val boundValueForm = basicFilterForm.bind("filters" -> "hello,world")
    assertEquals(boundValueForm.typedValueOpt, Option(Seq("hello", "world")))

    basicFilterForm.bind().fold(
      errorForm => {
        println(s"errorForm.errors = ${errorForm.errors}")
        fail("should not fail")
      },
      value => {
        assertEquals(value, Nil)
      }
    )
  }

  test("transform mapping from optional string to Seq[String]") {
    def basicFilterForm = Mapping(
      "filters", optional[String].transform[Seq[String]](
        opt =>
          opt.map(_.split(",").toSeq).getOrElse(Nil),
        l =>
          Option(l.mkString(", ")).filter(_.nonEmpty)
      )
    )

    val boundMissingValueForm = basicFilterForm.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, Option(Nil))

    val boundEmptyValueForm = basicFilterForm.bind("filters" -> "")
    assertEquals(boundEmptyValueForm.typedValueOpt, Option(Nil))

    val boundValueForm = basicFilterForm.bind("filters" -> "hello,world")
    assertEquals(boundValueForm.typedValueOpt, Option(Seq("hello", "world")))

    basicFilterForm.bind().fold(
      errorForm => {
        println(s"errorForm.errors = ${errorForm.errors}")
        fail("should not fail")
      },
      value => {
        assertEquals(value, Nil)
      }
    )
  }

  test("mapping to case class with transform") {
    case class SearchParam(query: String, filters: Seq[String] = Nil)

    val basicFilterForm: Mapping[SearchParam] = mapping[SearchParam](
      "query" -> default(""),
      "filters" -> optional[String].transform[Seq[String]](
        opt => opt.map(_.split(",").toSeq).getOrElse(Nil),
        l => Option(l.mkString(", ")).filter(_.nonEmpty)
      )
    )

    val boundForm = basicFilterForm.bind("query" -> "jake")

    boundForm.fold(
      errorForm => {
        println(s"errorForm.errors = ${errorForm.errors}")
        fail("should not fail")
      },
      value => {
        assertEquals(value, SearchParam(query = "jake"))
      }
    )
  }

  test("TransformMapping") {
    val form = nonEmptyText.transform[Option[LocalDateTime]](
      {
        case v if v.matches("(?i)now") => Option(LocalDateTime.ofInstant(Instant.EPOCH, ZoneId.of("UTC")))
        case v if v.matches("(?i)save as draft") => None
        case v if v.matches("(?i)1hr") => Option(LocalDateTime.now.plusHours(1))
        case v if v.matches("(?i)2hr") => Option(LocalDateTime.now.plusHours(2))
        case v if v.matches("(?i)1d") => Option(LocalDateTime.now.plusDays(1))
        case v if v.matches("(?i)1w") => Option(LocalDateTime.now.plusWeeks(1))
        case v => Option(LocalDateTime.parse(v))
      },
      {
        case Some(dt) if dt.toEpochSecond(ZoneOffset.UTC) == 0 => "Now"
        case Some(dt) => Option(dt.toString).getOrElse("Others")
        case _ => "Others"
      }
    ).name("schedule")

    val boundForm = form.bind("schedule" -> "Now")
    assertEquals(boundForm.typedValueOpt, Some(Some(LocalDateTime.ofInstant(Instant.EPOCH, ZoneId.of("UTC")))))

    val originalValue = boundForm.asInstanceOf[TransformMapping[String, Option[LocalDateTime]]].aMapping.typedValueOpt
    assertEquals(originalValue, boundForm.bindingValueOpt)

    assertEquals(boundForm.bindingValueOpt, Some("Now"))

    val filledForm = form.fill(Some(LocalDateTime.ofInstant(Instant.EPOCH, ZoneId.of("UTC"))))
    assertEquals(filledForm.bindingValueOpt, Some("Now"))

  }

  test("TransformMapping bind - optional field") {
    val form = Mapping(
      "values", optional(text).transform[List[String]](
        strOpt => strOpt.map(_.split("\\s*,\\s*").map(_.trim).toList).getOrElse(Nil),
        s => if s.nonEmpty then Some(s.mkString(",")) else None
      )
    )

    assertEquals(form.isRequired, false)

    //MissingValue
    val boundMissingValueForm = form.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, Option(Nil))
    assertEquals(boundMissingValueForm.bindingValueOpt, None)
    assertEquals(boundMissingValueForm.errors, Nil)
    assertEquals(boundMissingValueForm("values").typedValueOpt, Option(Nil))
    assertEquals(boundMissingValueForm("values").bindingValueOpt, None)
    assertEquals(boundMissingValueForm("values").errors, Nil)

    val boundEmptyValueForm = form.bind("values" -> "")
    assertEquals(boundEmptyValueForm.errors, Nil)
    assertEquals(boundEmptyValueForm("values").errors, Nil)
    assertEquals(boundEmptyValueForm.typedValueOpt, Option(Nil))
    assertEquals(boundEmptyValueForm("values").typedValueOpt, Option(Nil))
    assertEquals(boundEmptyValueForm("values").bindingValueOpt, Option(""))

    val emptyValueForm = form.fill(Nil)
    assertEquals(emptyValueForm.errors, Nil)
    assertEquals(emptyValueForm("values").errors, Nil)
    assertEquals(emptyValueForm.typedValueOpt, Option(Nil))
    assertEquals(emptyValueForm("values").typedValueOpt, Option(Nil))
    assertEquals(emptyValueForm("values").bindingValueOpt, None)
  }

  test("TransformMapping - required text to Option") {
    val form = Mapping("name", nonEmptyText.transform[Option[String]](s => Option(s), opt => opt.orNull))

    assertEquals(form.constraints.flatMap(_.name), Seq("constraint.required"))
    assertEquals(form("name").constraints.flatMap(_.name), Seq("constraint.required"))

    val diffTypeForm = Mapping("name", nonEmptyText.transform[String](s => s.take(4), s => s.take(4)))

    assertEquals(diffTypeForm.constraints.flatMap(_.name), Seq("constraint.required"))
    assertEquals(diffTypeForm("name").constraints.flatMap(_.name), Seq("constraint.required"))

    val tupSameTypeForm = tuple(
      "age" -> optional(number),
      "name" -> nonEmptyText.transform[Option[String]](s => Option(s), opt => opt.orNull)
    )

    assertEquals(tupSameTypeForm("name").constraints.flatMap(_.name), Seq("constraint.required"))

    val tupDiffTypeForm = tuple(
      "age" -> optional(number),
      "name" -> nonEmptyText.transform[Option[String]](s => Option(s), opt => opt.orNull)
    )

    assertEquals(tupDiffTypeForm("name").constraints.flatMap(_.name), Seq("constraint.required"))
  }

  test("test bind,fold with transform mapping using default") {
    val basicFilterForm = Mapping(
      "filters", default("").transform[Seq[String]](
        value => value.split(",").toIndexedSeq.filterNot(_.isEmpty),
        xs => xs.mkString(", ")
      )
    )
    assertEquals(basicFilterForm.bind().typedValueOpt, Option(Nil))
    assertEquals(basicFilterForm.bind("filters" -> "hello,world").typedValueOpt, Option(Seq("hello", "world")))
    assertEquals(basicFilterForm.bind("filters" -> "").typedValueOpt, Option(Nil))

    basicFilterForm.bind()
      .fold(errorForm => fail("should not fail"), value => assertEquals(value, Nil))
  }

  test("bind and fold with transform to case class with transform using default") {
    case class SearchParam(query: String, filters: Seq[String] = Nil)

    val basicFilterForm: Mapping[SearchParam] = mapping[SearchParam](
      "query" -> default(""),
      "filters" -> default("").transform[Seq[String]](
        value => value.split(",").toIndexedSeq.filterNot(_.isEmpty),
        xs => xs.mkString(", ")
      )
    )

    assertEquals(basicFilterForm.bind("query" -> "jake").typedValueOpt, Option(SearchParam("jake")))
    basicFilterForm.bind("query" -> "jake")
      .fold(errorForm => fail("should not fail"), value => assertEquals(value, SearchParam(query = "jake"))

      )
  }

  test("fill in transform field") {
    val form: Mapping[String] = Mapping(
      "age", number.transform[String](_.toString, _.toInt)
    )
    assert(form.fill("4").typedValueOpt.contains("4"))

    val form2: Mapping[Option[String]] = Mapping(
      "name", nonEmptyText.transform[Option[String]](str => Option(str), _.get)
    )
    assert(form2.fill(Some("hello")).typedValueOpt.contains(Some("hello"))) //hello

    val form3: Mapping[Seq[String]] = Mapping(
      "text", nonEmptyText.transform(_.split("\\s*,\\s*").toSeq, _.mkString(","))
    )
    assert(form3.fill(Seq("hello", "world")).typedValueOpt.contains(Seq("hello", "world"))) // "hello,world"
  }

  test("bind with JSON with transform".only){
    import com.greenfossil.commons.json.{*, given}
    val form: Mapping[(String, Option[JsValue])] = tuple(
      "action" -> nonEmptyText,
      "value" -> nonEmptyText
        .transform[Option[JsValue]](
          str => Try(Json.parse(str)).toOption,
          jsonOpt => jsonOpt.fold("")(_.toString)
        )
    )

    val value  =   Json.obj("a" -> "bar")
    val json = Json.obj("action" -> "foo", "value" -> value.stringify)
    form.bind(json).fold(
      error => fail(error.errors.map(_.message).mkString("")),
      {
        case (action, jsValue) =>
          assertNoDiff(action, "foo")
          assertEquals(jsValue, Some(value))
      }
    )
  }

}
