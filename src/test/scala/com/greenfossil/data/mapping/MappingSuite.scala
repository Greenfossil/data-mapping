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

class MappingSuite extends munit.FunSuite {
  import Mapping.*

  test("verifying"){
    val f1 = number.name("f1")
      .verifying("Int must be greater than 10", x => x > 10)
    val boundField = f1.bind("f1" -> "4")
    assertNoDiff(boundField.errors.head.messages.head , "Int must be greater than 10")
  }

  test("no such key"){
    val f1 = Mapping.tuple(
      "key1"-> text,
      "key2" -> text
    )
    val filledForm = f1.fill(("someValue", "someValue2"))
    assertEquals(filledForm("key1").typedValueOpt, Some("someValue"))
    assertEquals(filledForm("key2").typedValueOpt, Some("someValue2"))
    assertEquals(filledForm("key3"), null)
  }

}
