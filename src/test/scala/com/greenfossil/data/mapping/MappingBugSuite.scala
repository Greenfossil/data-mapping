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

import com.greenfossil.data.mapping.Mapping.*

class MappingBugSuite extends munit.FunSuite {


  test("filled in form should not have errors"){
    val form: Mapping[Int] = Mapping("age", number.verifying("minimum age is 10", _ > 10))

    val filledForm = form.fill(4)
    assert(filledForm.errors.isEmpty)

  }

  test("fill form for delegate mapping should not have errors") {
    def issueForm = Mapping("subject", nonEmptyText)

    val filledIssueForm = issueForm.fill("")

    assert(filledIssueForm.errors.isEmpty)

  }

  test("fill form for product mapping should not have errors"){
    def form = tuple("left" -> number, "right" -> number)
      .verifying("Left must be >= right", {case t: (Int, Int) => t._1 >= t._2})

    val filledForm = form.fill(0, 5)
    assert(filledForm.errors.isEmpty)
  }

  test("fill form for seq[Long] should not have errors"){
    def form = seq[Long].name("projects").verifying("Please select project", ids => ids.nonEmpty)
    val filledForm = form.fill(Nil)
    assertEquals(filledForm.errors, Nil)
  }

  test("nonEmptyText verifying constraints"){
    val form = Mapping("name", nonEmptyText.verifying("must be less than 5 characters", _.length < 5))
    val emptyBindedField = form.bind("name" -> "")
    assert(emptyBindedField.errors.nonEmpty)

    val missingBindedField2 = form.bind()
    assert(missingBindedField2.hasErrors)

    val invalidBindedField = form.bind("name" -> "mary sue")
    assert(invalidBindedField.errors.nonEmpty)
  }

  test("bind FieldMapping.apply(...) with text.bindname"){
    case class TestUser(name: String, userEmail: String)
    val form = mapping[TestUser](
      "name" -> text,
      "userEmail" -> text.bindName("email")
    )

    val boundForm = form.bind("name" -> "Homer", "email" -> "support@greenfossil.com")
    assertEquals(boundForm.typedValueOpt, Option(TestUser("Homer", "support@greenfossil.com")))
    assertEquals(boundForm("email").typedValueOpt, Option("support@greenfossil.com"))
  }

  test("get constraints and errors from form"){
    val form = Mapping("firstname", nonEmptyText)
    assertEquals(form.constraints.flatMap(_.name), Seq("constraint.required"))
    assertEquals(form("firstname").constraints.flatMap(_.name), Seq("constraint.required"))

    val boundErrorForm = form.bind()
    assertEquals(boundErrorForm.errors.map(_.message), Seq("error.required"))
    assertEquals(boundErrorForm("firstname").errors.map(_.message), Seq("error.required"))

    val nestedForm = Mapping(
     "name", tuple(
       "firstname" -> nonEmptyText,
       "lastname" -> nonEmptyText
     )
    )
    assertEquals(nestedForm("name.firstname").constraints.flatMap(_.name), Seq("constraint.required"))
    assertEquals(nestedForm("name.lastname").constraints.flatMap(_.name), Seq("constraint.required"))

    val boundErrorNestedForm = nestedForm.bind("name.firstname" -> "")
    assertEquals(boundErrorNestedForm("name.lastname").errors.map(_.message), Seq("error.required"))
    assertEquals(boundErrorNestedForm("name.firstname").errors.map(_.message), Seq("error.required"))

  }



  test("tuple mapping of required text") {
    val form = tuple(
      "firstname" -> text,
      "lastname" -> text
    )

    form.bind().fold(
      errorForm => {
        assertEquals(errorForm.errors.size, 2)
      },
      tup => fail(s"this should fail ${tup}")
    )
  }


  test("case class Name mapping of required text") {
    case class Name(firstname: String, lastname: String)
    val form = mapping[Name](
      "firstname" -> text,
      "lastname" -> text
    ).verifying("name must be homer", name => name.firstname == "homer")

    form.bind().fold(
      errorForm => {
        assertEquals(errorForm.errors.size, 2)
      },
      tup => fail (s"this should fail ${tup}")
    )
  }

  /**
   * Findings:
   * SeqMapping#boundFieldsWithPadding uses SeqMapping#boundValueIndexes to calculate the remaining fields to create.
   * But SeqMapping#boundValueIndexes is Nil if the field has error.
   * It should use SeqMapping#boundFields fields instead.
   *
   * Bug found in SeqMapping#boundFieldsWithPadding(int, scala.Function2) - Line 75
   *
   */
  test("SeqMapping.boundFieldsWithPadding should retain field count on error") {
    val form = Mapping("user",
      repeatedTuple(
        "lastname" -> nonEmptyText,
        "firstname" -> nonEmptyText,
      )
    )

    val bindForm = form.bind(Map("user[0].lastname" -> Nil, "user[0].firstname" -> Nil))
    val seqMapping = bindForm("user").asInstanceOf[SeqMapping[(String, String)]]
    val remappedBoundFields = seqMapping.boundFieldsWithPadding(1)([A] => (mapping: Mapping[A], row: Int) =>
      mapping.name(s"${mapping.bindingName}[$row]"))

    assertEquals(remappedBoundFields.size, 1)
  }

  test("SeqMapping.boundValueIndexes should show the correct value even if the form binding has errors"){
    val form = repeatedTuple(
      "id" -> longNumber,
      "name" -> nonEmptyText
    ).name("users")

    val bindForm = form.bind(
      "users[0].id" -> "", "users[0].name" -> "Homer",
      "users[1].id" -> "", "users[1].name" -> "Marge",
    )
    assertEquals(bindForm.boundValueIndexes.size, 2)

    val filledForm = form.fill(Seq(1L -> "Homer", 2L -> "Marge"))
    assertEquals(filledForm.boundValueIndexes.size, 2)
  }

}
