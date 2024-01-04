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

class MappingBind8_CheckedMappingSuite extends munit.FunSuite {
  import Mapping.*

  test("checked type") {
    val field: Mapping[Boolean] = checked("Please check this field").name("field") //Assign name to field
    //Bind Missing value
    val boundMissingValueField = field.bind()
    assertEquals(boundMissingValueField.typedValueOpt, Some(false))
    assertEquals(boundMissingValueField.bindingValueOpt, None)
    assertEquals(boundMissingValueField.errors.map(_.message), Seq("Please check this field"))

    //Bind Empty value
    val boundEmptyValueField = field.bind("field" -> "")
    assertEquals(boundEmptyValueField.typedValueOpt, Some(false))
    assertEquals(boundEmptyValueField.bindingValueOpt, Some(""))
    assertEquals(boundEmptyValueField.errors.map(_.message), Seq("Please check this field"))

    //Bind True value
    val boundTrueValueField = field.bind("field" -> "true")
    assertEquals(boundTrueValueField.typedValueOpt, Option(true))
    assertEquals(boundTrueValueField.bindingValueOpt, Option("true"))
    assertEquals(boundTrueValueField.errors, Nil)

    //Bind False value
    val boundFalseValueField = field.bind("field" -> "false")
    assertEquals(boundFalseValueField.typedValueOpt, Some(false))
    assertEquals(boundFalseValueField.bindingValueOpt, Option("false"))
    assertEquals(boundFalseValueField.errors.map(_.message), Seq("Please check this field"))

    //Bind non boolean value
    val boundNonBooleanField = field.bind("field" -> "test")
    assertEquals(boundNonBooleanField.typedValueOpt, Some(false))
    assertEquals(boundNonBooleanField.bindingValueOpt, Option("test"))
    assertEquals(boundNonBooleanField.errors.map(_.message), Seq("error.boolean"))
  }

  test("invalid default, checked type") {
    val form: Mapping[(String, Boolean)] = tuple(
      "defaultText" -> default(text, "Foo"),
      "isChecked" -> checked("this should be checked")
    )

    val boundForm = form.bind("defaultText" -> "Bar", "isChecked" -> "true")
    boundForm.fold(
      _ => fail("Should not have error"),
      data =>
        assertEquals(data, ("Bar", true))
    )
  }

  test("valid default, checked type") {
    val form: Mapping[(String, Boolean)] = tuple(
      "defaultText" -> default(text, "Foo"),
      "isChecked" -> checked("this should be checked")
    )

    val filledForm = form.bind("defaultText" -> "", "isChecked" -> "true") //Note: Binding value will not be null
    filledForm.fold(
      _ => fail("should not have errors"),
      data => {
        assertEquals(data, ("Foo", true))
      }
    )
  }

}
