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

import com.greenfossil.commons.json.Json

import scala.language.implicitConversions

class FormNestedFieldsSuite extends munit.FunSuite {
  import Mapping.*

  test("bind case-class field") {
    case class Address(postalCode: String, country: String)
    val form: Mapping[(Long, Address)] = tuple(
      "id" -> longNumber,
      "address" -> mapping[Address](
          "postalCode" -> text,
          "country" -> text
      )
    )

    assertNoDiff(form("id").tpe, "Long")
    assertNoDiff(form("address").tpe, "P+")

    val boundForm = form.bind(
      "id" -> "1",
      "address.postalCode" -> "123456",
      "address.country" -> "Singapore"
    )

    assertEquals(boundForm.typedValueOpt, Some((1L, Address("123456", "Singapore"))))

  }

  test("bind repeating case class field") {
    case class Address(postalCode: String, country: String)
    val form: Mapping[(Long, Seq[Address])] = tuple(
      "id" -> longNumber,
      "address" -> repeatedMapping[Address](
        "postalCode" -> text,
        "country" -> text
      )
    )

    assertNoDiff(form("id").tpe, "Long")
    assertNoDiff(form("address").tpe, "[Seq")

    val boundForm = form.bind(
      "id" -> "1",
      "address[0].postalCode" -> "123456",
      "address[0].country" -> "Singapore"
    )

    assertEquals(boundForm.typedValueOpt, Some((1L, Seq(Address("123456", "Singapore")))))

  }

  test("bind tuple field") {
    val form: Mapping[(Long, (String, String))] = tuple(
      "id" -> longNumber,
      "address" -> tuple(
        "postalCode" -> text,
        "country" -> text
      )
    )

    assertNoDiff(form("id").tpe, "Long")
    assertNoDiff(form("address").tpe, "P-")

    val boundForm = form.bind(
      "id" -> "1",
      "address.postalCode" -> "123456",
      "address.country" -> "Singapore"
    )

    assertEquals(boundForm.typedValueOpt, Some((1L, ("123456", "Singapore"))))

  }

  test("bind repeat tuple field") {
    val form: Mapping[(Long, Seq[(String, String)])] = tuple(
      "id" -> longNumber,
      "address" -> repeatedTuple(
        "postalCode" -> text,
        "country" -> text
      )
    )

    assertNoDiff(form("id").tpe, "Long")
    assertNoDiff(form("address").tpe, "[Seq")

    val boundForm = form.bind(
      "id" -> "1",
      "address[0].postalCode" -> "123456",
      "address[0].country" -> "Singapore"
    )

    assertEquals(boundForm.typedValueOpt, Some((1L, Seq(("123456", "Singapore")))))

  }
  
}
