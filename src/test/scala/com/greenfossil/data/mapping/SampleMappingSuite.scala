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

class SampleMappingSuite extends munit.FunSuite {
  import Mapping.* 

  test("required string") {

    val field = text.name("field")

    //Default field - Not filled and Not Bind yet
    assertEquals(field.typedValueOpt, None)
    assertEquals(field.bindingValueOpt, None)
    assertEquals(field.errors, Nil)

    //Filled field
    val filledField = field.fill("Hello World")
    assertEquals(filledField.typedValueOpt, Some("Hello World"))
    assertEquals(filledField.bindingValueOpt, Some("Hello World"))
    assertEquals(filledField.errors, Nil)

    //Bind Missing Field - likely for API
    val boundMissingField = field.bind()
    assertEquals(boundMissingField.typedValueOpt, None)
    assertEquals(boundMissingField.bindingValueOpt, None)
    assertEquals(boundMissingField.errors.head.message, "error.required") //all fields by default is required

    //Bind Empty Value  - like from Browser Input
    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.typedValueOpt, Option(""))
    assertEquals(boundEmptyField.bindingValueOpt, Option(""))
    assertEquals(boundEmptyField.errors, Nil)

    //Bind Value
    val boundValueField = field.bind("field" -> "hello world") //Note, we can use the bindingValue from Fill output
    assertEquals(boundValueField.typedValueOpt, Option("hello world"))
    assertEquals(boundValueField.bindingValueOpt, Option("hello world"))
    assertEquals(boundValueField.errors, Nil)
  }

  test("optional string") {

    val field = optional(text.name("field"))

    //Default field - Not filled and Not Bind yet
    assertEquals(field.typedValueOpt, None)
    assertEquals(field.bindingValueOpt, None)
    assertEquals(field.errors, Nil)

    //Filled field
    val filledField = field.fill(Option("Hello World"))
    assertEquals(filledField.typedValueOpt, Option(Option("Hello World")))
    assertEquals(filledField.bindingValueOpt, Option("Hello World"))
    assertEquals(filledField.errors, Nil)

    //Bind Missing Field - likely for API
    val boundMissingField = field.bind()
    assertEquals(boundMissingField.typedValueOpt, Some(None))
    assertEquals(boundMissingField.bindingValueOpt, None)
    assertEquals(boundMissingField.errors, Nil) //No errors as this is optional

    //Bind Empty Value  - like from Browser Input
    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.typedValueOpt, Option(None))
    assertEquals(boundEmptyField.bindingValueOpt, Option(""))
    assertEquals(boundEmptyField.errors, Nil)

    //Bind Value
    val boundValueField = field.bind("field" -> "hello world") //Note, we can use the bindingValue from Fill output
    assertEquals(boundValueField.typedValueOpt, Option(Option("hello world")))
    assertEquals(boundValueField.bindingValueOpt, Option("hello world"))
    assertEquals(boundValueField.errors, Nil)
  }

  test("required temporal") {

    val field = localTimeUsing("HH:mm:ss").name("field")

    //Default field - Not filled and Not Bind yet
    assertEquals(field.typedValueOpt, None)
    assertEquals(field.bindingValueOpt, None)
    assertEquals(field.errors, Nil)

    //Filled field
    val whenValue = "13:30:00" //DateTimeFormatter.ofPattern("HH:mm:ss").format(when)
    val when = java.time.LocalTime.parse(whenValue) //java.time.LocalTime.now
    val filledField = field.fill(when)
    assertEquals(filledField.typedValueOpt, Option(when))
    assertEquals(filledField.bindingValueOpt, Option(whenValue))
    assertEquals(filledField.errors, Nil)

    //Bind Missing Field - likely for API
    val boundMissingField = field.bind()
    assertEquals(boundMissingField.typedValueOpt, None)
    assertEquals(boundMissingField.bindingValueOpt, None)
    assertEquals(boundMissingField.errors.head.message, "error.required") //all fields by default is required

    //Bind Empty Value  - like from Browser Input
    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.typedValueOpt, None)
    assertEquals(boundEmptyField.bindingValueOpt, Option(""))
    assertEquals(boundEmptyField.errors.head.message, "error.localTime")

    //Bind Value
    val boundValueField = field.bind("field" -> whenValue) //Note, we can use the bindingValue from Fill output
    assertEquals(boundValueField.typedValueOpt, Option(java.time.LocalTime.parse(whenValue)))
    assertEquals(boundValueField.bindingValueOpt, Option(whenValue))
    assertEquals(boundValueField.errors, Nil)
  }

}
