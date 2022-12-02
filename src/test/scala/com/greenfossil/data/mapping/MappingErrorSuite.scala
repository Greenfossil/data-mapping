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

class MappingErrorSuite extends munit.FunSuite{
  import Mapping.*
  
  test("Field errors"){
    val boundForm = tuple("name" -> nonEmptyText, "value" -> text).bind("name" -> "", "value" -> "")
    assertEquals(boundForm.error("name").map(_.message),  Some("error.required"))
  }

  test("Form.withError"){
    val form = tuple("name" -> nonEmptyText, "value" -> text).withError("name", "error.required")
    assertEquals(form.error("name").map(_.message),  Some("error.required"))
  }

  test("Form.globalErrors"){
    val form = tuple("name" -> nonEmptyText, "value" -> text)
    val boundErrorForm = form.bind().withGlobalError("This is a sample error")

    assertEquals(boundErrorForm.globalErrors.headOption.map(_.message),  Some("This is a sample error"))
    assertEquals(boundErrorForm("name").errors.map(_.message).sorted, Seq("error.required").sorted)
    assertEquals(boundErrorForm.discardingErrors.apply("name").errors, Nil)
    assertEquals(boundErrorForm.filterErrors(e => e.key == "error.required").apply("name").errors, Nil)
    assert(boundErrorForm.hasGlobalErrors)
  }

  test("Form.globalErrors with single field"){
    val form = Mapping("name", nonEmptyText)
    val boundErrorForm = form.bind().withGlobalError("This is a sample error")
    assertEquals(boundErrorForm.globalErrors.map(_.message), Seq("This is a sample error"))
    assertEquals(boundErrorForm("name").errors.map(_.message).sorted, Seq("error.required", "This is a sample error").sorted)
    assertEquals(boundErrorForm("name").discardGlobalErrors.errors.map(_.message).sorted, Seq("error.required").sorted)
    assert(boundErrorForm.hasGlobalErrors)
  }

  test("Form.discardingErrors"){
    val errorForm = tuple("name" -> nonEmptyText, "value" -> text).bind("name" -> "", "value" -> "")
    assert(errorForm.hasErrors)
    assertEquals(errorForm.discardingErrors.hasErrors, false)
  }

}
