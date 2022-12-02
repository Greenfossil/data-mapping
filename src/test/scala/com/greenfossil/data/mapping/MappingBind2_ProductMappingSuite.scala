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

/*
 * Test Tuple, CaseClass Field
 */
class MappingBind2_ProductMappingSuite extends munit.FunSuite {
  import Mapping.*
  import com.greenfossil.commons.json.Json

  import java.time.LocalDate

  test("unnamed tuple"){
    val tf: Mapping[(String, Int)] = tuple(
      "f1" -> text,
      "f2" -> number
    )

    val f1 = tf("f1")
    val f2 = tf("f2")
    assertEquals(tf.apply("f1").typedValueOpt, None)
  }

  test("Tuple Mapping 2 Fields") {
    val tupleField: Mapping[(String, Int)] = tuple(
      "f1" -> text,
      "f2" -> number
    ).name("tupleField")

    val boundMissingValueForm = tupleField.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, None)
    assertEquals(boundMissingValueForm.boundValueIndexes, Nil)
    assertEquals(boundMissingValueForm.bindingValueOpt, None)

    val boundEmptyValueForm = tupleField.bind("f1" -> "", "f2" -> "")
    assertEquals(boundEmptyValueForm.typedValueOpt, None)
    assertEquals(boundEmptyValueForm.boundValueIndexes, Nil)
    assertEquals(boundEmptyValueForm.bindingValueOpt, None)

    val boundField = tupleField.bind("tupleField.f1" -> "Hello", "tupleField.f2" -> "1")
    assertEquals(boundField.typedValueOpt, Some(("Hello", 1)))
    assertEquals(boundField.boundValueIndexes, Seq(0))
    assertEquals(boundField.boundValueOf(0),Some(("Hello", 1)))
    println(s"boundField.bindingValue = ${boundField.bindingValueOpt}")
  }

  test("CaseClass Mapping 2 fields") {
    case class Contact(name: String, number: Int)
    val tupleField: Mapping[Contact] = mapping[Contact](
      "name" -> text,
      "number" -> number
    ).name("contact")

    val boundField = tupleField.bind("contact.name" -> "Hello", "contact.number" -> "1")
    assertEquals(boundField.typedValueOpt, Some(Contact("Hello", 1)))
  }

  test("bind tuple 2") {
    val form: Mapping[(Long, String, Seq[Int])] = tuple(
      "long" -> longNumber,
      "text" -> text,
      "seq" -> seq[Int]
    )
    val boundForm = form.bind("long" -> "1", "text" -> "hello", "seq[1]" -> "1", "seq[2]" -> "2")
    val a = boundForm("long")
    val x = boundForm("long").typedValueOpt
    assertEquals[Any, Any](boundForm("long").typedValueOpt, Option(1))
    assertEquals[Any, Any](boundForm("text").typedValueOpt, Option("hello"))
    assertEquals[Any, Any](boundForm("seq").typedValueOpt, Option(Seq(1, 2)))

    assertEquals(boundForm.typedValueOpt, Option((1L, "hello", Seq(1, 2))))
  }

  test("bind tuple 3") {
    val form = tuple(
      "name" -> text,
      "birthday" -> localDate
    )

    val boundForm = form.bind("name" -> "Homer", "birthday" -> "1990-01-01")
    assertEquals[Any, Any](boundForm("birthday").typedValueOpt, Some(LocalDate.parse("1990-01-01")))
    assertEquals(boundForm.typedValueOpt, Some(("Homer", LocalDate.parse("1990-01-01"))))
  }

  test("valid bind and fold") {

    val form: Mapping[(Long, String, Seq[Long])] = tuple(
      "l" -> longNumber,
      "s" -> text,
      "xs" -> seq[Long]
    )

    form.bind("l" -> "1", "s" -> "text", "xs[1]" -> "1", "xs[2]" -> "2")
      .fold(
        errorForm => fail("Should not have error form"),
        data => assertEquals(data, (1L, "text", Seq(1L, 2L)))
      )
  }

  test("invalid bind and fold") {

    val form: Mapping[(Long, String, Seq[Long])] = tuple(
      "l" -> longNumber(1, 2, true),
      "s" -> text,
      "xs" -> seq[Long]
    )

    form.bind("l" -> "10", "s" -> "text", "xs[1]" -> "1", "xs[2]" -> "2").fold(
      errorForm => {
        assertEquals(errorForm.errors.size, 1)
      },
      data => fail("should not return invalid data")
    )
  }

  test("bind tuple 1") {
    val form = text.name("name").bind("name" -> "Homer")
    assertEquals(form.typedValueOpt, Some("Homer"))

    form.fold(
      errorForm => fail("Should not have error"),
      name => {
        assertNoDiff(name, "Homer")
      }
    )
  }

  case class UserData(name: String, age: Int)

  test("case class Form - field-based verify") {
    val userFormConstraints =
      mapping[UserData](
        "name" -> nonEmptyText,
        "age" -> number.verifying("Age must be 42", age => age == 42)
      )

    val boundForm = userFormConstraints.bind("name" -> "1", "age" -> "10")
    assertEquals(boundForm.hasErrors, true)
    assertNoDiff(boundForm.errors.head.messages.head, "Age must be 42")
  }

  test("case class Form - form-based verifying success") {
    val userFormConstraints =
      mapping[UserData](
        "name" -> text,
        "age" -> number
      ).verifying("Name should be homer and age is 42", user => user.name == "homer" && user.age == 42)
    val boundForm = userFormConstraints.bind("name" -> "homer", "age" -> "10")
    assertEquals(boundForm.hasErrors, true)
    assertNoDiff(boundForm.errors.head.messages.head, "Name should be homer and age is 42")
  }

  test("case class Form - form-based verifying failure") {
    val userFormConstraints =
      mapping[UserData](
        "name" -> text,
        "age" -> number
      ).verifying("Name should be homer and age is 42", user => user.name == "homer" && user.age == 42)
    val boundForm = userFormConstraints.bind("name" -> "homer", "age" -> "42")
    assertEquals(boundForm.hasErrors, false)
  }

  test("form verifying") {
    val userFormConstraints: Mapping[UserData] = mapping[UserData](
      "name" -> text,
      "age" -> number
    ).verifying("Bad data", userData => true)

    val tupleField: Mapping[(String, Int)] = tuple(
      "name" -> text, //FIXME .transform[Int](s => s.toInt, int => int.toString),
      "age" -> number
    ).verifying("Bad data", tuple => true)

  }

  test("form seq binding with verifying") {
    val optionalField = seq[String].verifying("Test error", xs => xs.exists(s => s.equals("test")))
    val form = optionalField.name("foo")

    assertEquals(form.bind("foo[1]" -> "test").typedValueOpt, Some(Seq("test")))
    assert(form.bind("foo[1]" -> "abc").errors.exists(_.message == "Test error"))
  }

  test("tuple with both error field and bound field"){
    val form = tuple(
      "name" -> nonEmptyText,
      "dob" -> localDateUsing("dd/MM/yyyy")
    )
    val boundForm = form.bind("name" -> "Homer")
    assertEquals(boundForm.typedValueOpt, None)
  }

  test("tuple with verifying"){
    val form: Mapping[(String, String)] = tuple(
      "firstname" -> nonEmptyText,
      "lastname" -> nonEmptyText
    ).verifying(
      "Simpsons other than Homer are not allowed",
      tup => tup(1) != "Simpson" || tup(0) == "Homer"
    )

    val filledForm = form.bind("firstname" -> "Marge", "lastname" -> "Simpson")
    assert(filledForm.hasErrors)
    assertEquals(filledForm.errors.map(_.message), Seq("Simpsons other than Homer are not allowed"))
    assertEquals(filledForm("firstname").typedValueOpt, Some("Marge"))
    assertEquals(filledForm("lastname").typedValueOpt, Some("Simpson"))
    assertEquals(filledForm.typedValueOpt, Some(("Marge", "Simpson")))
  }

  test("case class mapping"){
    case class SearchParam(query: String, entityViewerId: Option[String] = None, filters: Seq[String] = Nil, pageNumber: Int = 1, pageSize: Int = 10, context: Map[String, Any] = Map.empty)
    val searchParamForm: Mapping[SearchParam] = mapping[SearchParam](
      "query" -> default(""),
      "entityViewerId" -> optional[String],
      "filters" -> default("").transform[Seq[String]](
        str => Option(str).filter(_.nonEmpty).map(_.split(",").toSeq).getOrElse(Nil),
        l => l.mkString(", ")
      ),
      "pageNumber" -> default(1).bindName("page"),
      "pageSize" -> default(10),
      "context" -> ignored[Map[String, Any]](Map.empty[String, Any])
    )

    val boundForm = searchParamForm.bind("query" -> "tan")
    assertEquals(boundForm.errors, Nil)
    assertEquals(boundForm.typedValueOpt, Option(SearchParam("tan")))
  }

}
