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

class MappingBind7_IgnoredMappingSuite extends munit.FunSuite {

  import Mapping.*

  test("ignored") {

    val field = ignored(2L).name("field")

    //Fil value
    val filledField = field.fill(5)
    assertEquals(filledField.typedValueOpt, Option(2L))
    assertEquals(filledField.bindingValueOpt, Option("2"))
    assertEquals(filledField.errors, Nil)

    //Bind missing Value
    val boundMissingValueField = field.bind()
    assertEquals(boundMissingValueField.typedValueOpt, Option(2L))
    assertEquals(boundMissingValueField.bindingValueOpt, None)
    assertEquals(boundMissingValueField.errors, Nil)

    val boundEmptyValueFied = field.bind("field" -> "")
    assertEquals(boundEmptyValueFied.typedValueOpt, Option(2L))
    assertEquals(boundEmptyValueFied.bindingValueOpt, Option(""))
    assertEquals(boundEmptyValueFied.errors, Nil)

    val boundField = field.bind("field" -> "5")
    assertEquals(boundField.typedValueOpt, Option(2L))
    assertEquals(boundField.bindingValueOpt, Some("5"))
    assertEquals(boundField.errors, Nil)
  }

  test("ignored with None as default"){
    val form = ignored(Option.empty[String]).name("opt")

    val missingBoundForm = form.bind()
    assertEquals(missingBoundForm.typedValueOpt, Option(None))
    assertEquals(missingBoundForm.errors, Nil)

    val emptyValueBoundForm = form.bind("opt" -> "")
    assertEquals(emptyValueBoundForm.typedValueOpt, Option(None))
    assertEquals(emptyValueBoundForm.errors, Nil)

    val valueBoundForm = form.bind("opt" -> "invalid")
    assertEquals(valueBoundForm.typedValueOpt, Option(None))
    assertEquals(valueBoundForm.errors, Nil)
  }


  test("bind to ignored[Map]") {
    Mapping("context", ignored(Map.empty[String, Any])).bind().fold(
      errorForm => {
        fail(s"should not have errors ${errorForm}")
      },
      contextMap => {
        assertEquals(contextMap, Map.empty[String, Any])
      }
    )
  }

}
