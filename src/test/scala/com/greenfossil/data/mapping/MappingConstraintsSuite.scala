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

class MappingConstraintsSuite extends munit.FunSuite {
  import Mapping.*
  
  /*
   * For implementing constraints - use play.api.data.Forms for help. Need to use Constraints class
   */

  test("nonEmptyText") {
    val f = nonEmptyText.name("f")
    val errorField = f.bind("f" -> "")
    assertEquals(errorField.errors.size, 1)
    assertEquals(errorField.errors.head.message, "error.required")

    val validField = f.bind("f" -> "abc")
    assertEquals(validField.errors, Nil)
  }

  test("email constraints") {
    val form = email.name("email")
    assertEquals(form.bind("email" -> "test email@greenfossil.com").hasErrors, true)
    assertEquals(form.bind("email" -> "test@greenfossil.com").hasErrors, false)
  }

  test("email"){
    val f = email.name("f")
    val errorField = f.bind("f" -> "https://www.google.com")
    assertEquals(errorField.errors.size, 1)
    val errorField2 = f.bind("f" -> "greenfossil.com")
    assertEquals(errorField2.errors.size, 1)
    val validField = f.bind("f" -> "test@greenfossil.com")
    assert(validField.errors.isEmpty)
  }

  test("phone"){
    val f = phone.name("f")
    assertEquals(f.bind("f" -> "12+88776655").errors.head.message, "error.phone")
    assertEquals(f.bind("f" -> "+6588776655").errors, Nil)
    assertEquals(f.bind("f" -> "6588776655").errors, Nil)
    assertEquals(f.bind("f" -> "88776655").errors, Nil)
    assertEquals(f.bind("f" -> "66776655").errors, Nil)
  }

  test("mobilePhone"){
    val f = mobilePhone.name("f")
    assertEquals(f.bind("f" -> "12+88776655").errors.head.message, "error.mobile")
    assertEquals(f.bind("f" -> "66554422").errors.head.message, "error.mobile")
    assertEquals(f.bind("f" -> "88776655").errors, Nil)
    assertEquals(f.bind("f" -> "99776655").errors, Nil)
  }

  test("number"){
    val f = number(1, 10).name("f")

    val errorField = f.bind("f" -> "20")
    assertEquals(errorField.errors.size, 1)
    val validField = f.bind("f" -> "5")

    assert(validField.errors.isEmpty)
  }

  test("text with trim option"){
    val f = text(1, 5, true).name("f")

    val validField = f.bind("f" -> "hello")
    assert(validField.errors.isEmpty)

    val errorField = f.bind("f" -> "hello world")
    assertEquals(errorField.errors.size, 1)

    val errorField2 = f.bind("f" -> "    ")
    assertEquals(errorField2.errors.size, 1)
    assertEquals(errorField2.typedValueOpt, Option(""))

    val validField2 = f.bind("f" -> "hello ")
    assert(validField2.errors.isEmpty)
    assertEquals(validField2.typedValueOpt, Option("hello"))
  }

  test("text without trim option"){
    val f = text(1, 5, false).name("f")

    val errorField = f.bind("f" -> "hello world")
    assertEquals(errorField.errors.size, 1)

    val errorField2 = f.bind("f" -> "hello ")
    assertEquals(errorField2.errors.size, 1)

    val validField = f.bind("f" -> "hello")
    assert(validField.errors.isEmpty)

    val validField2 = f.bind("f" -> "    ")
    assert(validField.errors.isEmpty)
  }

  test("byte number"){
    val f = byteNumber(min = 2, max = 8).name("f")

    val errorField = f.bind("f" -> 10.toByte.toString)
    assertEquals(errorField.errors.size, 1)

    val validField = f.bind("f" -> "8")
    assert(validField.errors.isEmpty)
  }

  test("short number"){
    val nonStrictField = shortNumber(1,10, false).name("f")

    val nonStrictValidField = nonStrictField.bind("f" -> "1")
    assertEquals(nonStrictValidField.errors.size, 0)

    val nonStrictErrorField = nonStrictField.bind("f" -> "0")
    assertEquals(nonStrictErrorField.errors.size, 1)
    assertEquals(nonStrictErrorField.errors.head.message, "error.min")

    val strictField = shortNumber(1,10, true).name("f")
    val strictValidField = strictField.bind("f" -> "2")
    assertEquals(strictValidField.errors.size, 0)

    val strictErrorField = strictField.bind("f" -> "0")
    assertEquals(strictErrorField.errors.size, 1)
    assertEquals(strictErrorField.errors.head.message, "error.min.strict")

  }

  test("long number"){
    val nonStrictField = longNumber(1,10, false).name("f")

    val nonStrictValidField = nonStrictField.bind("f" -> "1")
    assertEquals(nonStrictValidField.errors.size, 0)

    val nonStrictErrorField = nonStrictField.bind("f" -> "0")
    assertEquals(nonStrictErrorField.errors.size, 1)
    assertEquals(nonStrictErrorField.errors.head.message, "error.min")

    val strictField = longNumber(1,10, true).name("f")
    val strictValidField = strictField.bind("f" -> "2")
    assertEquals(strictValidField.errors.size, 0)

    val strictErrorField = strictField.bind("f" -> "0")
    assertEquals(strictErrorField.errors.size, 1)
    assertEquals(strictErrorField.errors.head.message, "error.min.strict")

  }

  test("big decimal"){
    val field = bigDecimal(1,1).name("f")
    val errorField = field.bind("f" -> "1") // scale is 0, precision is 1
    assertEquals(errorField.errors.size, 1)
    assertEquals(errorField.errors.head.message, "error.real.precision")

    val validField = field.bind("f" -> "0.1") // scale is 1, precision is 1
    assertEquals(validField.errors.size, 0)

  }

  test("custom constraint"){
    val field = text
      .name("f")
      .verifying("text needs to be alphanumerical", _.matches("[a-zA-Z0-9]*"))

    val errorField = field.bind("f" -> "@#$")
    assertEquals(errorField.errors.size, 1)
    assertEquals(errorField.errors.head.message, "text needs to be alphanumerical")

    val validField = field.bind("f" -> "asdf1234")
    assert(validField.errors.isEmpty)
  }
}
