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

class MappingCaseClassBindNameSuite extends munit.FunSuite {
  import Mapping.*
  
  case class UserData(name: String, age: Int)

  test("case class Form - field-based verify") {
    val userFormConstraints =
      mapping[UserData](
        "name" -> nonEmptyText.bindName("My-Name"),
        "age"  -> number.bindName("My-Age").verifying("Age must be 42", age => age == 42)
      )

    assertEquals(userFormConstraints.fieldNames, Seq("name", "age"))
    assertEquals(userFormConstraints.fieldBindingNames, Seq("My-Name", "My-Age"))
    val boundForm = userFormConstraints.bind("My-Name" -> "1", "My-Age" -> "10")
    assertEquals(boundForm.hasErrors, true)
    assertNoDiff(boundForm.errors.head.messages.head, "Age must be 42")
  }

  test("case class Form - form-based verifying success") {
    val userFormConstraints =
      mapping[UserData](
        "name" -> text,
        "age"  -> number
      ).verifying("Name should be homer and age is 42", user => user.name == "homer" && user.age == 42)
    val boundForm = userFormConstraints.bind("name" -> "homer", "age" -> "10")
    assertEquals(boundForm.hasErrors, true)
    assertNoDiff(boundForm.errors.head.messages.head, "Name should be homer and age is 42")
  }

  test("case class Form - form-based verifying failure") {
    val userFormConstraints =
      mapping[UserData](
        "name" -> text,
        "age"  -> number
      ).verifying("Name should be homer and age is 42", user => user.name == "homer" && user.age == 42)
    val boundForm = userFormConstraints.bind("name" -> "homer", "age" -> "42")
    assertEquals(boundForm.hasErrors, false)
  }

  test("form seq binding with verifying"){
    val optionalField = seq[String].verifying("Test error", xs => xs.exists(s => s.equals("test")))
    val form = optionalField.name("foo").bindName("Foo")

    assertEquals(form.bind("Foo[1]" -> "test").typedValueOpt, Some(Seq("test")))
    assert(form.bind("Foo[1]" -> "abc").errors.exists(_.message == "Test error"))
  }

}
