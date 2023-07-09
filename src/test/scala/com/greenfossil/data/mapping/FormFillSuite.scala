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

import java.time.LocalDate
import scala.language.implicitConversions

class FormFillSuite extends munit.FunSuite {

  import Mapping.*

  test("tuple 2") {
    val form = tuple(
      "long" -> longNumber,
      "text" -> text
    )
    val filledForm = form.fill(1, "hello")
    assertEquals[Any, Any](filledForm("long").typedValueOpt, Option(1))
    assertEquals[Any, Any](filledForm("text").typedValueOpt, Option("hello"))

    assertEquals(filledForm.typedValueOpt, Some((1L, "hello")))
  }

  test("fill tuple") {
    val form = tuple(
      "long" -> longNumber,
      "text" -> text,
      "seq" -> seq[Long]
    )
    val filledForm = form.fill(1, "hello", Seq(1,2))
    assertEquals[Any, Any](filledForm("long").typedValueOpt, Option(1L))
    assertEquals[Any, Any](filledForm("text").typedValueOpt, Option("hello"))
    assertEquals[Any, Any](filledForm("seq").typedValueOpt, Option(Seq(1,2)))

    assertEquals(filledForm.typedValueOpt, Option((1L, "hello", Seq(1L, 2L))))
  }

  test("bind tuple 3"){
    val form = tuple(
      "name" -> text,
      "birthday" -> localDate
    )
    val filledForm = form.fill("Homer", LocalDate.parse("1990-01-01"))
    assertEquals[Any, Any](filledForm("birthday").typedValueOpt, Some(LocalDate.parse("1990-01-01")))
  }

  test("case class 2") {
    case class Foo(l: Long, s: String)
    val form = mapping[Foo](
      "l" -> longNumber,
      "s" -> text
    )
    val filledForm = form.fill(Foo(1, "hello"))
    assertEquals[Any, Any](filledForm("l").typedValueOpt, Option(1))
    assertEquals[Any, Any](filledForm("s").typedValueOpt, Option("hello"))

  }

  test("case class 3") {
    case class Foo(l: Long, s: String, xs: Seq[Long])
    val form: Mapping[Foo] = mapping[Foo](
      "l" -> longNumber,
      "s" -> text,
      "xs" -> seq[Long]
    )
    val filledForm = form.fill(Foo(1, "hello", Seq(1,2)))
    assertEquals[Any, Any](filledForm("l").typedValueOpt, Option(1))
    assertEquals[Any, Any](filledForm("s").typedValueOpt, Option("hello"))
    assertEquals[Any, Any](filledForm("xs").typedValueOpt, Option(Seq(1,2)))

  }

  test("valid form fill"){
    val form: Mapping[(Long, String, Seq[Long])] = tuple(
      "l" -> longNumber,
      "s" -> text,
      "xs" -> seq[Long]
    )
    val filledForm = form.fill((1L, "text", Seq(1L, 2L)))
    assertEquals[Any, Any](filledForm("l").typedValueOpt, Some(1L))
    assertEquals[Any, Any](filledForm("s").typedValueOpt, Some("text"))
    assertEquals[Any, Any](filledForm("xs").typedValueOpt, Some(Seq(1L, 2L)))
  }

}
