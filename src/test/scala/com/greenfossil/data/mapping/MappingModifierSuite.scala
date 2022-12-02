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

class MappingModifierSuite extends munit.FunSuite{
  import Mapping.*
  
  test("set field name"){
    val f1 = number.name("f1")
    val f2 = number.name("f1")
    assertNoDiff(f1.name, f2.name)
  }

  test("set tuple field name") {
    val tup = tuple(
      "f1" -> text,
      "f2" -> number
    ).name("tupField")
    assertNoDiff(tup.name, "tupField")
  }

  test("set mapping field name"){
    case class Foo(f1: String, f2: Int)
    val mappingField = mapping[Foo](
      "f1" -> text,
      "f2" -> number
    ).name("Foo")
    assertNoDiff(mappingField.name, "Foo")
  }

  test("seq field name"){
    val f1 = seq[Int].name("f1")
    assertNoDiff(f1.name, "f1")
    assertNoDiff(f1.asInstanceOf[SeqMapping[Int]].elemField.name, "f1")
  }


}
