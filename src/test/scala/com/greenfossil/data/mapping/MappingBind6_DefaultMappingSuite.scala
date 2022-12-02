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

class MappingBind6_DefaultMappingSuite extends munit.FunSuite {
  import Mapping.*

  import java.time.*

  test("default String type") {
    val field = default(text, "Foo").name("field") // Assigned a name to the default field

    //Without binding
    assertEquals(field.typedValueOpt, Option("Foo"))
    assertEquals(field.bindingValueOpt, Some("Foo"))

    //Bind with missing value
    val boundMissingValueField = field.bind()
    assertEquals(boundMissingValueField.typedValueOpt, Option("Foo"))
    assertEquals(boundMissingValueField.bindingValueOpt, None)

    //Bind with empty value
    val boundEmptyValueField = field.bind("field" -> "")
    assertEquals(boundEmptyValueField.typedValueOpt, Option("Foo"))
    assertEquals(boundEmptyValueField.bindingValueOpt, Option(""))

    val boundValueField = field.bind("field" -> "Bar")
    assertEquals(boundValueField.typedValueOpt, Option("Bar"))
    assertEquals(boundValueField.bindingValueOpt, Option("Bar"))
  }

  test("default number") {
    val form = Mapping("age", default[Int](50))

    //Bind Missing Value
    val boundMissingValueField = form.bind()
    assert(boundMissingValueField.errors.isEmpty)
    assertEquals(boundMissingValueField.typedValueOpt, Option(50))

    //Bind Empty Value
    val boundEmptyValueField = form.bind("age" -> "")
    assertEquals(boundEmptyValueField.typedValueOpt, Option(50))

    //Bind Value
    val boundValueField = form.bind("age" -> "10")
    assert(boundValueField.errors.isEmpty)
    assertEquals(boundValueField.typedValueOpt, Option(10))
  }

  test("default BigDecimal type") {
    val field: Mapping[BigDecimal] = default(bigDecimal, 100).name("default(bigDecimal)")

    //Bind Missing Value
    val boundMissingValueField = field.bind()
    assert(boundMissingValueField.errors.isEmpty)
    assertEquals(boundMissingValueField.typedValueOpt, Option(BigDecimal(100)))

    //Bind Empty Value
    val boundEmptyValueField = field.bind("default(bigDecimal)" -> "")
    assert(boundEmptyValueField.errors.isEmpty)
    assertEquals(boundEmptyValueField.typedValueOpt, Option(BigDecimal(100)))

    //Bind Value
    val boundValueField = field.bind("default(bigDecimal)" -> "100.0")
    assertEquals(boundValueField.typedValueOpt, Option(BigDecimal(100)))
    assertEquals(boundValueField.bindingValueOpt, Option("100.0"))

    //fill Value
    val filledValueField = field.fill(100)
    assertEquals(filledValueField.typedValueOpt, Option(BigDecimal(100)))
    assertEquals(filledValueField.bindingValueOpt, Option("100"))

  }

  test("default type with fill null") {
    val field = default(text, "Foo").name("field") // Assigned a name to the default field
    assertEquals(field.typedValueOpt, Option("Foo"))

    val filledField2 = field.fill(null)
    assertEquals(filledField2.typedValueOpt, Option("Foo"))
  }
  
  test("default string null"){
    val field = Mapping("name", default[String](null))
    
    val boundField = field.bind()
    assertEquals(boundField.typedValueOpt, Some(null))
  }

}
