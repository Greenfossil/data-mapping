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

class MappingBind2_ProductMappingJson_HtmlSuite extends munit.FunSuite {

  import Mapping.*
  import com.greenfossil.commons.json.*

  test("Json nested tuple fields") {
    val form: Mapping[(Long, (String, (String, String)))] = tuple(
      "id" -> longNumber,
      "address" -> tuple(
        "postalCode" -> text,
        "numList" -> tuple(
          "html1" -> htmlText,
          "html2" -> htmlText,
        )
      )
    )

    val boundVEmptyValueForm = form.bind(Json.obj("id" -> "", "address" -> ""))
    assertEquals(boundVEmptyValueForm.typedValueOpt, None)

    val boundVEmptyValue2Form = form.bind(Json.obj("id" -> "1", "address" -> ""))
    assertEquals[Any, Any](boundVEmptyValue2Form.typedValueOpt, None)

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "numList" -> Json.obj(
          "html1" -> "<script></script>",
          "html2" -> "<b>test</b><script>alert(1)</script>")
      )
    )

    val boundValueForm = form.bind(jsonObject)
    assertEquals(boundValueForm.typedValueOpt, Some((1L, ("123456", ("", "<b>test</b>")))))
  }

  test("case class 3") {
    case class Foo(l: Long, s: String, xs: Seq[String])
    val form: Mapping[Foo] = mapping[Foo](
      "l" -> longNumber,
      "s" -> htmlText,
      "xs" -> seq(htmlText)
    )

    val boundMappingForm1 = form.bind("l" -> "1", "s" -> "<b>test</b><script>alert(1)</script>",
      "xs[1]" -> "<script></script>",
      "xs[2]" -> "<script SRC=http://xss.rocks/xss.js></script>",
      "xs[3]" -> "<b>test</b><script>alert(1)</script>",
    )
    assertEquals[Any, Any](boundMappingForm1("l").typedValueOpt, Option(1))
    assertEquals[Any, Any](boundMappingForm1("s").typedValueOpt, Option("<b>test</b>"))
    assertEquals[Any, Any](boundMappingForm1("xs").typedValueOpt, Option(Seq("", "", "<b>test</b>")))

    val boundMappingForm2 = form.bind("l" -> "1",
      "s" -> "<b>test</b><script>alert(1)</script>",
      "xs[0]" -> "<b>test</b><script>alert(1)</script>",
      "xs[1]" -> "<b>test</b><script>alert(1)</script>")
    assertEquals[Any, Any](boundMappingForm2("l").typedValueOpt, Option(1))
    assertEquals[Any, Any](boundMappingForm2("s").typedValueOpt, Option("<b>test</b>"))
    assertEquals[Any, Any](boundMappingForm2("xs").typedValueOpt, Option(Seq("<b>test</b>", "<b>test</b>")))

    val jsonboundForm = form.bind(Json.obj("l" -> 1,
      "s" -> "<b>test</b><script>alert(1)</script>",
      "xs" -> Json.arr("<b>test</b><script>alert(1)</script>", "<b>test</b><script>alert(1)</script>")))
    assertEquals[Any, Any](jsonboundForm("l").typedValueOpt, Some(1))
    assertEquals[Any, Any](jsonboundForm("s").typedValueOpt, Some("<b>test</b>"))
    assertEquals[Any, Any](jsonboundForm("xs").typedValueOpt, Some(Seq("<b>test</b>", "<b>test</b>")))
  }

  test("bind as JSON: JsArray to repeatedTuple") {
    val form = Mapping("services",
      repeatedTuple(
        "name" -> nonEmptyHtmlText,
        "isDone" -> boolean
      )
    )
    val jsArray = Json.obj(
      "services" -> Json.arr(
        Json.obj("name" -> """<IMG onmouseover="alert('xxs')">""", "isDone" -> JsNull),
        Json.obj("name" -> "<b>test</b><script>alert(1)</script>", "isDone" -> "true"),
        Json.obj("name" -> "<svg onload='alert(1)'>", "isDone" -> "false"),
      )
    )

    val boundForm = form.bind(jsArray)
    assertEquals(boundForm.errors, Nil)
    assertEquals(boundForm.typedValueOpt.getOrElse(Nil),
      Seq("<IMG >" -> false, "<b>test</b>" -> true, "<svg >"-> false))
  }


}
