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

import scala.language.implicitConversions

class MappingBind2_ProductMappingJsonSuite extends munit.FunSuite {
  import Mapping.*
  import com.greenfossil.commons.json.*

  import java.time.*

  test("Json nested tuple fields") {
    val form: Mapping[(Long, (String,  (Long, Long)))] = tuple(
      "id" -> longNumber,
      "address" -> tuple(
        "postalCode" -> text,
        "numList" -> tuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val boundVEmptyValueForm = form.bind(Json.obj("id" -> "","address" -> ""))
    assertEquals(boundVEmptyValueForm.typedValueOpt, None)

    val boundVEmptyValue2Form = form.bind(Json.obj("id" -> "1", "address" -> ""))
    assertEquals[Any, Any](boundVEmptyValue2Form.typedValueOpt, None)

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "numList" -> Json.obj(
          "num1" -> 1,
          "num2" -> 2)
      )
    )

    val boundValueForm = form.bind(jsonObject)
    assertEquals(boundValueForm.typedValueOpt, Some((1L, ("123456", (1L, 2L)))))
  }

  test("Json nested tuple optional fields".fail) {
    val form: Mapping[(Option[Long], (String, (Long, Long)))] = tuple(
      "id" -> optional(longNumber),
      "address" -> tuple(
        "postalCode" -> text,
        "numList" -> tuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val boundVEmptyValueForm = form.bind(Json.obj("id" -> "", "address" -> ""))
    assertEquals(boundVEmptyValueForm.typedValueOpt, null)

    val boundVEmptyValue2Form = form.bind(Json.obj("id" -> "1", "address" -> ""))
    assertEquals[Any, Any](boundVEmptyValue2Form.typedValueOpt, Some((Some(1), null)))

    val boundVEmptyValue3Form = form.bind(
      Json.obj(
        "id" -> "",
        "address" -> Json.obj("postalCode" -> "123456"))
    )
    assertEquals[Any, Any](boundVEmptyValue3Form.typedValueOpt, Some((None, ("123456", null))))

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "numList" -> Json.obj(
          "num1" -> 1,
          "num2" -> 2)
      )
    )

    val boundValueForm = form.bind(jsonObject)
    assertEquals(boundValueForm.typedValueOpt, Some((Some(1L), ("123456", (1L, 2L)))))
  }

  test("Json nested tuple optional fields 2") {
    val form: Mapping[(Option[Long], (String, Option[(Long, Long)]))] = tuple(
      "id" -> optional(longNumber),
      "address" -> tuple(
        "postalCode" -> text,
        "numList" -> optionalTuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val boundVEmptyValueForm = form.bind(Json.obj("id" -> "", "address" -> ""))
    assertEquals(boundVEmptyValueForm.typedValueOpt, None)

    val boundVEmptyValue2Form = form.bind(Json.obj("id" -> "1", "address" -> ""))
        assertEquals[Any, Any](boundVEmptyValue2Form.typedValueOpt, None)

    val boundVEmptyValue3Form = form.bind(
      Json.obj(
        "id" -> "",
        "address" -> Json.obj("postalCode" -> "123456"))
    )
        assertEquals[Any, Any](boundVEmptyValue3Form.typedValueOpt, Some((None, ("123456", None))))

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "numList" -> Json.obj(
          "num1" -> 1,
          "num2" -> 2)
      )
    )

    val boundValueForm = form.bind(jsonObject)
    assertEquals(boundValueForm.typedValueOpt, Some((Some(1L), ("123456", Some((1L, 2L))))))
  }

  test("bind as JSON") {
    val form = tuple(
      "name" -> text,
      "age" -> number,
      "isActive" -> boolean,
      "id" -> longNumber,
      "balance" -> float,
      "remaining" -> double,
      "birthday" -> localDate
    )
    val jsonObject = Json.obj(
      "name" -> "Homer", "age" -> 50, "isActive" -> true, "id" -> 123456L, "balance" -> 100.12F,
      "remaining" -> 100.00, "birthday" -> LocalDate.parse("1990-01-01")
    )

    val boundForm = form.bind(jsonObject)
    assertEquals[Any, Any](boundForm("name").typedValueOpt, Some("Homer"))
    assertEquals[Any, Any](boundForm("age").typedValueOpt, Some(50))
    assertEquals[Any, Any](boundForm("isActive").typedValueOpt, Some(true))
    assertEquals[Any, Any](boundForm("id").typedValueOpt, Some(123456L))
    assertEquals[Any, Any](boundForm("balance").typedValueOpt, Some(100.12F))
    assertEquals[Any, Any](boundForm("remaining").typedValueOpt, Some(100.00))
    assertEquals[Any, Any](boundForm("birthday").typedValueOpt, Some(LocalDate.parse("1990-01-01")))

    assertEquals(boundForm.typedValueOpt, Some(("Homer", 50, true, 123456L, 100.12F, 100.00, LocalDate.parse("1990-01-01"))))

  }

  test("case class 3") {
    case class Foo(l: Long, s: String, xs: Seq[Long])
    val form: Mapping[Foo] = mapping[Foo](
      "l" -> longNumber,
      "s" -> text,
      "xs" -> seq[Long]
    )

    val boundMappingForm1 = form.bind("l" -> "1", "s" -> "hello", "xs[1]" -> "1", "xs[2]" -> "2")
    assertEquals[Any, Any](boundMappingForm1("l").typedValueOpt, Option(1))
    assertEquals[Any, Any](boundMappingForm1("s").typedValueOpt, Option("hello"))
    assertEquals[Any, Any](boundMappingForm1("xs").typedValueOpt, Option(Seq(1, 2)))

    val boundMappingForm2 = form.bind("l" -> "1", "s" -> "hello", "xs[0]" -> "1", "xs[1]" -> "2")
    assertEquals[Any, Any](boundMappingForm2("l").typedValueOpt, Option(1))
    assertEquals[Any, Any](boundMappingForm2("s").typedValueOpt, Option("hello"))
    assertEquals[Any, Any](boundMappingForm2("xs").typedValueOpt, Option(Seq(1, 2)))

    val jsonboundForm = form.bind(Json.obj("l" -> 1, "s" -> "hello", "xs" -> Json.arr(1, 2)))
    assertEquals[Any, Any](jsonboundForm("l").typedValueOpt, Some(1))
    assertEquals[Any, Any](jsonboundForm("s").typedValueOpt, Some("hello"))
    assertEquals[Any, Any](jsonboundForm("xs").typedValueOpt, Some(Seq(1, 2)))
  }

  test("bind as JSON: JsObject to tuple") {
    val form = tuple(
      "name" -> text,
      "age" -> number,
      "isActive" -> boolean,
      "id" -> longNumber,
      "balance" -> float,
      "remaining" -> double,
      "birthday" -> localDate
    )
    val jsonObject = Json.obj(
      "name" -> "Homer", "age" -> 50, "isActive" -> true, "id" -> 123456L, "balance" -> 100.12F,
      "remaining" -> 100.00, "birthday" -> LocalDate.parse("1990-01-01")
    )

    val boundForm = form.bind(jsonObject)
    assertEquals[Any, Any](boundForm("name").typedValueOpt, Some("Homer"))
    assertEquals[Any, Any](boundForm("age").typedValueOpt, Some(50))
    assertEquals[Any, Any](boundForm("isActive").typedValueOpt, Some(true))
    assertEquals[Any, Any](boundForm("id").typedValueOpt, Some(123456L))
    assertEquals[Any, Any](boundForm("balance").typedValueOpt, Some(100.12F))
    assertEquals[Any, Any](boundForm("remaining").typedValueOpt, Some(100.00))
    assertEquals[Any, Any](boundForm("birthday").typedValueOpt, Some(LocalDate.parse("1990-01-01")))

    assertEquals(boundForm.typedValueOpt, Some(("Homer", 50, true, 123456L, 100.12F, 100.00, LocalDate.parse("1990-01-01"))))
    assertEquals(boundForm.noOfFields, 7)
  }

  test("bind as JSON: JsArray to repeatedTuple"){
    val form = Mapping("services",
      repeatedTuple(
        "name" -> nonEmptyText,
        "isDone" -> boolean
      )
    )
    val jsArray = Json.obj(
      "services" -> Json.arr(
        Json.obj("name" -> "Today's Admission", "isDone" -> "true"),
        Json.obj("name" -> "Bed Allocation", "isDone" -> "false"),
        Json.obj("name" -> "Payment", "isDone" -> JsNull),
      )
    )

    val boundForm = form.bind(jsArray)
    assertEquals(boundForm.errors, Nil)
    assertEquals(boundForm.typedValueOpt.getOrElse(Nil).sorted,
      Seq("Today's Admission" -> true, "Bed Allocation" -> false, "Payment" -> false).sorted)
  }

  test("bind as JSON: JsArray to seq[String]") {
    val form = Mapping("services", seq(nonEmptyText))
    val jsArray = Json.obj(
      "services" -> Json.arr("Today's Admission", "Bed Allocation")
    )

    val boundForm = form.bind(jsArray)
    assertEquals(boundForm.errors, Nil)
    assertEquals(boundForm.typedValueOpt.getOrElse(Nil).sorted, Seq("Today's Admission", "Bed Allocation").sorted)
  }

  test("bind from json with optional fields"){
    val form = tuple("name" -> nonEmptyText, "dob" -> optional(localDateUsing("yyyy-MM-dd")))
    val json = Json.obj("name" -> "homer", "dob" -> "")
    val jsonBoundForm = form.bind(json)
    val regularBoundForm = form.bind("name" -> "homer", "dob" -> "")

    assertEquals(jsonBoundForm.errors, regularBoundForm.errors)
    assertEquals(jsonBoundForm.typedValueOpt, regularBoundForm.typedValueOpt)
  }
}
