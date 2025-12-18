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

import com.greenfossil.commons.json.Json

import java.time.LocalDate

/*
 * Test for Option and Seq Field
 */
class MappingBind3_OptionalMappingSuite extends munit.FunSuite {

  import Mapping.*

  test("Json nested tuple fields") {

    val form: Mapping[Option[Long]] = optional(longNumber).name("id")

    assertEquals(form.fieldBindingNames, Seq("id"))

    val boundMissingValueForm = form.bind(Json.obj())
    assertEquals(boundMissingValueForm.typedValueOpt, Some(None))

    val boundEmptyValueForm = form.bind(Json.obj("id" -> ""))
    assertEquals(boundEmptyValueForm.typedValueOpt, Some(None))

    val boundVEmptyValue2Form = form.bind(Json.obj("id" -> "1"))
    assertEquals[Any, Any](boundVEmptyValue2Form.typedValueOpt, Some(Some(1)))
  }

  test("optional string") {
    val field: Mapping[Option[String]] = optional(text).name("field")

    val boundMissingField = field.bind()
    assertEquals(boundMissingField.errors, Nil)
    assertEquals(boundMissingField.typedValueOpt, Some(None))
    assertEquals(boundMissingField.bindingValueOpt, None)

    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField.typedValueOpt, Some(None))
    assertEquals(boundEmptyField.bindingValueOpt, Some(""))

    val boundValueField = field.bind("field" -> "Hello World!")
    assertEquals(boundValueField.errors, Nil)
    assertEquals(boundValueField.typedValueOpt, Some(Some("Hello World!")))
    assertEquals(boundValueField.bindingValueOpt, Some("Hello World!"))

  }

  test("optional int") {
    val field = optional(number.name("field"))

    val boundMissingField = field.bind()
    assertEquals(boundMissingField.errors, Nil)
    assertEquals(boundMissingField.typedValueOpt, Some(None))
    assertEquals(boundMissingField.bindingValueOpt, None)

    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField.typedValueOpt, Some(None))
    assertEquals(boundEmptyField.bindingValueOpt, Some(""))

    val boundValueField = field.bind("field" -> "1")
    assertEquals(boundValueField.errors, Nil)
    assertEquals(boundValueField.typedValueOpt, Some(Some(1)))
    assertEquals(boundValueField.bindingValueOpt, Some("1"))
  }

  test("fill form for optional(long) should not have errors") {
    def form = optional(longNumber)
      .name("projectId")
      .verifying("Please select project", ids => ids.forall(_ > 0L))

    val filledForm = form.fill(Some(0))
    assertEquals(filledForm.errors, Nil)
  }

  test("optional") {
    val form: Mapping[(Long, Option[String])] = Mapping.tuple(
      "id" -> longNumber,
      "address" -> optional[String]
    )
    val filledForm = form.fill(1, Option("test"))
    assertEquals(filledForm.typedValueOpt, Some((1L, Some("test"))))
  }

  test("optional with constraints") {
    val form: Mapping[(Long, Option[String])] = Mapping.tuple(
      "id" -> longNumber,
      "address" -> optional(text(0, 255, true))
    )
    val filledForm = form.fill(1, Option("test"))
    assertEquals(filledForm.typedValueOpt, Some((1L, Some("test"))))
  }

  test("Option[Int]") {
    val optIntField: Mapping[Option[Int]] =  optional[Int].name("optInt")
    val boundField = optIntField.bind("optInt" -> "1")
    assertEquals(boundField.typedValueOpt, Option(Option(1)))
    assertEquals(boundField.boundValueIndexes, Seq(0))
    assertEquals(boundField.boundValueOf(0), Option(1)) //FIXME
  }

  test("Option[String]") {
    val optIntField = optional[String].name("optString")
    val boundField = optIntField.bind("optString" -> "Hello World!")
    assertEquals(boundField.typedValueOpt, Option(Option("Hello World!")))
  }

  test("Option(text)"){
    val optText: Mapping[Option[String]] = Mapping("optText", optional(text))

    val boundMissingField = optText.bind()
    assertEquals(boundMissingField.typedValueOpt, Some(None))
    assertEquals(boundMissingField.errors, Nil)
    assertEquals(boundMissingField("optText").errors, Nil)

    val boundEmptyField = optText.bind("optText" -> "")
    assertEquals(boundEmptyField.typedValueOpt, Option(None))
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField("optText").errors, Nil)

    val boundFieldWithText = optText.bind("optText" -> "Hello World!")
    assertEquals(boundFieldWithText.typedValueOpt, Option(Option("Hello World!")))
    assertEquals(boundFieldWithText.errors, Nil)
    assertEquals(boundFieldWithText("optText").errors, Nil)

  }

  test("Option[Tuple]") {
    val tupleField: Mapping[Option[(String, Int)]] = optionalTuple(
      "name" -> text,
      "contact" -> number
    ).name("tupleField")

    val boundField = tupleField.bind("tupleField.name" -> "Hello World!", "tupleField.contact" -> "123")
    assertEquals(boundField.typedValueOpt, Option(Option(("Hello World!", 123))))
  }

  test("Option[Mapping]") {
    case class Contact(name: String, contact: Int)
    val tupleField: Mapping[Option[Contact]] = optionalMapping[Contact](
      "name" -> text,
      "contact" -> number
    ).name("tupleField")

    val boundField = tupleField.bind("tupleField.name" -> "Hello World!", "tupleField.contact" -> "123")
    assertEquals(boundField.typedValueOpt, Option(Option(Contact("Hello World!", 123))))
  }

  test("2 levels optional mapping with value") {
    case class User(id: Long,  address: Option[Address])
    case class Address(postalCode: String, country: String)

    val nestedOptionalField :Mapping[Option[User]] = optionalMapping[User](
      "id" -> longNumber,
      "address" -> optionalMapping[Address](
        "postalCode" -> text,
        "country" -> text
      )
    ).name("nestedOptionalField")

    val boundField = nestedOptionalField.bind("nestedOptionalField.id" -> "1",
      "nestedOptionalField.address.postalCode" -> "123",
      "nestedOptionalField.address.country" -> "Singapore"
    )
    assertEquals(boundField.typedValueOpt, Option(Option(User(1L, Option(Address("123", "Singapore"))))))
  }

  test("2 levels optional mapping without value") {
    case class User(id: Long,  address: Option[Address])
    case class Address(postalCode: String, country: String)

    val nestedOptionalField :Mapping[Option[User]] = optionalMapping[User](
      "id" -> longNumber,
      "address" -> optionalMapping[Address](
        "postalCode" -> text,
        "country" -> text
      )
    ).name("nestedOptionalField")

    val boundField = nestedOptionalField.bind("nestedOptionalField.id" -> "1")
    assertEquals(boundField.typedValueOpt, Option(Option(User(1L, None))))
  }

  test("top level optional mapping, inner level repeatMapping with values") {
    case class User(id: Long,  address: Seq[Address])
    case class Address(postalCode: String, country: String)

    val nestedOptionalField :Mapping[Option[User]] = optionalMapping[User](
      "id" -> longNumber,
      "address" -> repeatedMapping[Address](
        "postalCode" -> text,
        "country" -> text
      )
    ).name("nestedOptionalField")

    val boundField = nestedOptionalField.bind("nestedOptionalField.id" -> "1",
      "nestedOptionalField.address.postalCode" -> "123",
      "nestedOptionalField.address.country" -> "Singapore"
    )
    assertEquals(boundField.typedValueOpt, Option(Option(User(1L, Seq(Address("123", "Singapore"))))))
  }

  test("top level optional mapping, inner level repeatMapping without values") {
    case class User(id: Long,  address: Seq[Address])
    case class Address(postalCode: String, country: String)

    val nestedOptionalField :Mapping[Option[User]] = optionalMapping[User](
      "id" -> longNumber,
      "address" -> repeatedMapping[Address](
        "postalCode" -> text,
        "country" -> text
      )
    ).name("nestedOptionalField")

    val boundField = nestedOptionalField.bind("nestedOptionalField.id" -> "1")
    assertEquals(boundField.typedValueOpt, Option(Option(User(1L, Nil))))
  }
  

  test("form optional binding with verifying") {
    val optionalField = optional[String]
      .verifying("Test error", strOpt => strOpt.exists(s => s.equals("test")))

    val form = optionalField.name("foo").bindName("Foo")

    assertEquals(form.bind("Foo" -> "test").typedValueOpt, Option(Option("test")))
    assert(form.bind("Foo" -> "abc").errors.exists(_.message == "Test error"))
  }

  test("form optional binding with verifying 2") {
    val optionalField = optional[String]
      .verifying("Test error", strOpt => strOpt.exists(s => s.equals("test")))

    assertEquals(optionalField.typedValueOpt, None)

    val form = optionalField.name("foo")

    val boundMissingValueForm = form.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, Option(None))
    assertEquals(boundMissingValueForm.errors.map(_.message),  Seq("Test error")) //Apply constraints

    val boundEmptyForm = form.bind("foo" -> "")
    assertEquals(boundEmptyForm.typedValueOpt, Option(None))
    assertEquals(boundEmptyForm.errors.map(_.message), Seq("Test error")) //Apply constraints
  }


  test("bind tuple 1 optional[String]") {
    val form = optional[String].name("name")
    assertEquals[Any, Any](form.bind("name" -> "Homer").typedValueOpt, Option(Option("Homer")))
  }

  test("bind optional[String]") {
    val form = tuple("name" -> optional[String], "age" -> optional[Int])
    val boundForm = form.bind("name" -> "Homer", "age" -> "50")
    assertEquals(boundForm.typedValueOpt, Some((Option("Homer"), Option(50))))

    boundForm.fold(
      _ => fail("should not have error"),
      {
        case (nameOpt, ageOpt) =>
          assertEquals(nameOpt, Option("Homer"))
          assertEquals(ageOpt, Option(50))
      }
    )
  }

  test("bind optional(text)") {
    val form = tuple("name" -> optional(text), "age" -> optional(number))
    val boundForm = form.bind("name" -> "Homer", "age" -> "50")
    assertEquals(boundForm.typedValueOpt, Some((Option("Homer"), Option(50))))

    boundForm.fold(
      _ => fail("should not have error"),
      {
        case (nameOpt, ageOpt) =>
          assertEquals(nameOpt, Option("Homer"))
          assertEquals(ageOpt, Option(50))
      }
    )
  }

  test("bind optional with no value") {
    val form = tuple("name" -> optional(text), "age" -> optional(number))
    val boundForm = form.bind("name" -> "Homer")
    assertEquals(boundForm.typedValueOpt, Some((Option("Homer"), None)))

    boundForm.fold(
      _ => fail("should not have error"),
      {
        case (nameOpt, ageOpt) =>
          assertEquals(nameOpt, Option("Homer"))
          assertEquals(ageOpt, None)
      }
    )
  }

  test("bind tuple 4 - longnumber") {
    val form = tuple(
      "l1" -> longNumber,
      "l2" -> optional[Long]
    )
    val boundForm = form.bind("l1" -> "1", "l2" -> "")
    assertEquals(boundForm.typedValueOpt, Some((1L, None)))

    val boundForm2 = form.bind("l1" -> "1") //test absence of l2
    assertEquals(boundForm2.typedValueOpt, Some((1L, None)))
  }

  test("Json optional nested tuple fields") {
    val form: Mapping[(Long, (String, String, Option[(Long, Long)]))] = tuple(
      "id" -> longNumber,
      "address" -> tuple(
        "postalCode" -> text,
        "country" -> text,
        "numList" -> optionalTuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val jsonObject = Json.obj(
      "id" -> "1",
      "country" -> "Singapore",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "country" -> "Singapore",
        "numList" -> Json.obj(
          "num1" -> 1,
          "num2" -> 2)
      )
    )

    val boundForm = form.bind(jsonObject)
    assertEquals(boundForm.typedValueOpt, Some((1L, ("123456", "Singapore", Some((1L, 2L))))))
  }

  test("Json optional nested tuple fields with no values") {
    val form: Mapping[(Long, (String, String, Option[(Long, Long)]))] = tuple(
      "id" -> longNumber,
      "address" -> tuple(
        "postalCode" -> text,
        "country" -> text,
        "numList" -> optionalTuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "country" -> "Singapore"
      )
    )

    val boundForm = form.bind(jsonObject)
    assertEquals(boundForm.typedValueOpt, Some((1L, ("123456", "Singapore", None))))
  }

  test("Json optional nested mapping fields with values") {

    case class User(id: Long, address: Address)
    case class Address(postalCode: String, country: String, numList: Option[(Long, Long)])

    val form: Mapping[User] = mapping[User](
      "id" -> longNumber,
      "address" -> mapping[Address](
        "postalCode" -> text,
        "country" -> text,
        "numList" -> optionalTuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "country" -> "Singapore",
        "numList" -> Json.obj(
          "num1" -> 1,
          "num2" -> 2)
      )
    )

    val boundForm = form.bind(jsonObject)
    assertEquals(boundForm.typedValueOpt, Some(User(1L, Address("123456", "Singapore", Some((1L, 2L))))))
  }

  test("Json optional nested mapping fields with no values") {
    case class User(id: Long, address: Address)
    case class Address(postalCode: String, country: String, numList: Option[(Long, Long)])

    val form: Mapping[User] = mapping[User](
      "id" -> longNumber,
      "address" -> mapping[Address](
        "postalCode" -> text,
        "country" -> text,
        "numList" -> optionalTuple(
          "num1" -> longNumber,
          "num2" -> longNumber,
        )
      )
    )

    val jsonObject = Json.obj(
      "id" -> "1",
      "address" -> Json.obj(
        "postalCode" -> "123456",
        "country" -> "Singapore"
      )
    )

    val boundForm = form.bind(jsonObject)
    assertEquals(boundForm.typedValueOpt, Some(User(1L, Address("123456", "Singapore", None))))
  }

  test("optional fields should allow empty") {
    val form = tuple(
      "age" -> optional(number),
      "name" -> optional(nonEmptyText)
    )

    val boundMissingValueField3 = form.bind() //No value to bind, hence no errrors
    assert(boundMissingValueField3.errors.isEmpty)

    val boundEmptyValueField = form.bind("age" -> "", "name" -> "")
    assertEquals(boundEmptyValueField.errors, Nil)

    val boundField = form.bind("age" -> "8", "name" -> "homer")
    assert(boundField.errors.isEmpty)
  }

  test("optional field can use fold") {
    val form: Mapping[Option[String]] = Mapping("name", optional(text))

    form.bind("name" -> "homer").fold(
      errorForm => {
        fail(s"should not have errors ${errorForm}")
      },
      nameOpt => {
        assertEquals(nameOpt, Option("homer"))
      }
    )

    val boundForm = form.bind("age" -> "")
    assertEquals(boundForm.typedValueOpt, Some(None))

    form.bind("age" -> "1").fold(
      errorForm => {
        fail(s"should not have errors ${errorForm}")
      },
      nameOpt =>
        assertEquals(nameOpt, None)
    )
  }

  test("tuple with optional inner field") {
    val field = Mapping("student", tuple(
      "name" -> nonEmptyText,
      "age" -> optional(number)
    ))

    val boundNameField = field.bind("student.name" -> "homer")
    assertEquals(boundNameField.typedValueOpt, Option("homer" -> None))
    assert(!boundNameField.hasErrors, "should not have errors")

    val boundField = field.bind("student.name" -> "homer", "student.age" -> "8")
    assertEquals(boundField.typedValueOpt, Option("homer" -> Option(8)))
    assert(!boundField.hasErrors, "should not have errors")
  }

  test("fill optional number field") {
    val form = Mapping("age", optional(number))

    assertEquals(form.fill(Some(8)).typedValueOpt, Option(Some(8)))
    assertEquals(form.fill(None).typedValueOpt, Option(None))
  }

  test("bind optional number field") {
    val form = Mapping("age", optional(number))

    val boundMissingValueForm = form.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, Some(None))
    assertEquals(boundMissingValueForm.errors, Nil)

    val boundEmptyValueForm = form.bind("age" -> "")
    assertEquals(boundEmptyValueForm.typedValueOpt, Some(None)) //Should not have an error
    assertEquals(boundEmptyValueForm.errors, Nil)

    val boundValueForm = form.bind("age" -> "8")
    assertEquals(boundValueForm.typedValueOpt, Option(Some(8)))
    assertEquals(boundValueForm.errors, Nil)
  }

  test("fill optional nonEmptyText") {
    val form = Mapping("name", optional(nonEmptyText))

    assertEquals(form.fill(None).typedValueOpt, Option(None))
    assertEquals(form.fill(Some("")).typedValueOpt, Option(Some("")))
    assertEquals(form.fill(Some("hello world")).typedValueOpt, Option(Some("hello world")))

  }

  test("bind optional nonEmptyText") {
    val form = Mapping("name", optional(nonEmptyText))

    //MissingValue
    val boundMissingValueForm = form.bind()
    assertEquals(boundMissingValueForm.typedValueOpt, Some(None))
    assertEquals(boundMissingValueForm.bind().errors, Nil)

    //EmptyValue
    val boundEmptyValue = form.bind("name" -> "")
    assertEquals(boundEmptyValue.errors, Nil)
    assertEquals(boundEmptyValue.typedValueOpt, Some(None))

    //With Value
    assertEquals(form.bind("name" -> "Homer").typedValueOpt, Option(Some("Homer")))
    assertEquals(form.bind("name" -> "Homer").errors, Nil)
  }

  test("bind optional(localdatetimeusing())") {
    val form = Mapping("date", optional(localDateUsing("yyyy-MM-dd")))

    //MissingValue
    assertEquals(form.bind().typedValueOpt, Option(None))
    assertEquals(form.bind().errors, Nil)

    assertEquals(form.bind("date" -> "").typedValueOpt, Option(None))
    assertEquals(form.bind("date" -> "").errors, Nil)

    assertEquals(form.bind("date" -> "2022-01-01").typedValueOpt, Option(Some(LocalDate.parse("2022-01-01"))))
    assertEquals(form.bind("date" -> "2022-01-01").errors, Nil)
  }

  test("bind optional(email)") {
    val form = Mapping("email", optional(email))

    assertEquals(form.bind().typedValueOpt, Option(None))
    assertEquals(form.bind().errors, Nil)

    assertEquals(form.bind("email" -> "").typedValueOpt, Option(None))
    assertEquals(form.bind("email" -> "").errors, Nil)

    assertEquals(form.bind("email" -> "support@greenfossil.com").typedValueOpt, Option(Some("support@greenfossil.com")))
    assertEquals(form.bind("email" -> "support@greenfossil.com").errors, Nil)

    assertEquals(form.bind("email" -> "greenfossil.com").typedValueOpt, Option(Some("greenfossil.com")))
    assertEquals(form.bind("email" -> "greenfossil.com").errors, List(MappingError("email", List("error.email"), Nil)))
  }

  test("bind ProductMapping.apply(...) with optional(text).bindname") {
    case class TestUser(name: String, emailOpt: Option[String])
    val form = mapping[TestUser](
      "name" -> text,
      "emailOpt" -> optional(text).bindName("email")
    )

    val boundForm = form.bind("name" -> "Homer", "email" -> "support@greenfossil.com")
    assertEquals(boundForm.typedValueOpt, Option(TestUser("Homer", Some("support@greenfossil.com"))))
    assertEquals(boundForm("email").typedValueOpt, Option(Some("support@greenfossil.com")))
  }

  test("tuple mapping of optional text") {
    val form = tuple(
      "firstname" -> optional(text),
      "lastname" -> optional(text)
    )

    form.bind().fold(
      errorForm => {
        fail(s"should not have errors ${errorForm}")
      },
      tup =>
        assertEquals(tup, (None, None))
    )
  }

  test("case class Name mapping of optional text") {
    case class Name(firstname: Option[String], lastname: Option[String])
    val form = mapping[Name](
      "firstname" -> optional(text),
      "lastname" -> optional(text)
    )

    form.bind().fold(
      errorForm => {
        fail(s"should not have errors ${errorForm}")
      },
      tup =>
        assertEquals(tup, Name(None, None))
    )
  }

  test("case class Name mapping of optional text with verify") {
    case class Name(firstname: Option[String], lastname: Option[String])
    val form = mapping[Name](
      "firstname" -> optional(text),
      "lastname" -> optional(text)
    ).verifying("name must be homer", name => name.firstname.contains("homer"))

    form.bind().fold(
      _ => (),
      _ => fail("should have errors")
    )
  }

  test("mapping of optional text") {
    val form = Mapping("firstname", optional(text))

    form.bind("firstname" -> "").fold(
      errorForm => {
        fail(s"should not have errors ${errorForm}")
      },
      data =>
        assertEquals(data, None)
    )
  }

  test("error messages from field") {
    val singleForm = Mapping(
      "name", nonEmptyText.verifying("Must be from Simpson Family", _.endsWith("Simpson"))
    )
    assertEquals(singleForm.bind().errors.map(_.message), Seq("error.required"))
    assertEquals(singleForm("name").bind().errors.map(_.message), Seq("error.required"))
    assertEquals(singleForm.bind("name" -> "").errors.map(_.message).sorted, Seq("Must be from Simpson Family", "error.required").sorted)
    assertEquals(singleForm("name").bind("name" -> "").errors.map(_.message).sorted, Seq("Must be from Simpson Family", "error.required").sorted)

    val tupleForm = tuple(
      "age" -> optional(number),
      "name" -> nonEmptyText.verifying("Must be from Simpson Family", _.endsWith("Simpson"))
    )

    assertEquals(tupleForm.bind().errors.map(_.message), Seq("error.required"))
    assertEquals(tupleForm("name").bind().errors.map(_.message), Seq("error.required"))

    assertEquals(tupleForm.bind("name" -> "").errors.map(_.message).sorted, Seq("Must be from Simpson Family", "error.required").sorted)
    assertEquals(tupleForm("name").bind("name" -> "").errors.map(_.message).sorted, Seq("Must be from Simpson Family", "error.required").sorted)
  }

  test("optionalTuple"){
    val form = Mapping(
      "contact", optionalTuple(
        "type" -> nonEmptyText,
        "value" -> nonEmptyText
      )
    )
    val boundEmptyField = form.bind("contact.type" -> "", "contact.value" -> "")
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField.typedValueOpt, Option(None))

    val missingBoundField = form.bind()
    assertEquals(missingBoundField.errors, Nil)
    assertEquals(missingBoundField.typedValueOpt, Option(None))

    val partialBoundField = form.bind("contact.type" -> "hello", "contact.value" -> "")
    assertEquals(partialBoundField.errors.map(_.message), Seq("error.required"))
    assertEquals(partialBoundField.typedValueOpt, Option(Option("hello", "")))

    val nonEmptyBoundField = form.bind("contact.type" -> "hello", "contact.value" -> "world")
    assertEquals(nonEmptyBoundField.errors, Nil)
    assertEquals(nonEmptyBoundField.typedValueOpt, Option(Option("hello", "world")))
  }

  test("optionalTuple with optional(longNumber) and verifying"){
    val form = Mapping(
      "user", optionalTuple(
        "name" -> nonEmptyText,
        "userId" -> optional(longNumber)
      ).verifying("Simpsons need to have userIds", _.forall{case (name, idOpt) => !name.endsWith("Simpson") || idOpt.nonEmpty})
    )

    val missingField = form.bind()
    assertEquals(missingField.errors, Nil)
    assertEquals(missingField.typedValueOpt, Option(None))

    val boundEmptyField = form.bind("user.name" -> "", "user.userId" -> "")
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField.typedValueOpt, Option(None))

    val invalidField = form.bind("user.name" -> "Simpson", "user.userId" -> "")
    assertEquals(invalidField.errors.map(_.message), Seq("Simpsons need to have userIds"))
    assertEquals(invalidField.typedValueOpt, Option(Option("Simpson", None)))

    val validField = form.bind("user.name" -> "Simpson", "user.userId" -> "3")
    assertEquals(validField.errors, Nil)
    assertEquals(validField.typedValueOpt, Option(Option("Simpson", Option(3L))))
  }

  test("optionalTuple with boolean"){
    val form = Mapping(
      "user", optionalTuple(
        "isMember" ->  boolean, //optional(nonEmptyText).transform[Boolean](opt => opt.contains("true"), bool => Option(bool.toString)),
        "userId" -> optional(longNumber),
        "name" -> nonEmptyText
      ).verifying("Members must have user ids", _.forall { case (isMember, userIdOpt, _) => !isMember || userIdOpt.nonEmpty })
    )

    val trueBoundForm = form.bind("user.name" -> "Homer", "user.isMember" -> "true", "user.userId" -> "3")
    assertEquals(trueBoundForm.errors, Nil)
    assertEquals(trueBoundForm.typedValueOpt, Option(Option((true, Option(3L), "Homer"))))

    val noneBoundForm = form.bind("invalid" -> "")
    assertEquals(noneBoundForm.typedValueOpt, Option(None))
    assertEquals(noneBoundForm.errors, Nil)
  }

  test("optional verifying"){
    val form = optional(text).name("verifier")
      .verifying("Reject code verifier, length must be min:43 characters or max:128 characters",
        x => x.forall { value => !(value.length < 43 || value.length > 128) }
      )

    val boundMissingValueForm = form.bind()
    assertEquals(boundMissingValueForm.errors, Nil)
    val boundEmptyValueForm = form.bind("verifier" -> "")
    assertEquals(boundEmptyValueForm.errors, Nil)
    val boundBadLowValueForm = form.bind("verifier" -> "1".repeat(42))
    assertEquals(boundBadLowValueForm.errors.map(_.message), List("Reject code verifier, length must be min:43 characters or max:128 characters"))
    val boundBadHighValueForm = form.bind("verifier" -> "1".repeat(129))
    assertEquals(boundBadHighValueForm.errors.map(_.message), List("Reject code verifier, length must be min:43 characters or max:128 characters"))
    val boundGoodValueForm = form.bind("verifier" -> "1".repeat(50))
    assertEquals(boundGoodValueForm.typedValueOpt, Some(Some("1".repeat(50))))
  }

  test("optional phone"){
    val form = optional(phone).bindName("phoneNo")
    assertEquals(form.bind("phoneNo" -> "abc88776655").hasErrors, true)
    assertEquals(form.bind("phoneNo" -> "88776655").errors, Nil)
    assertEquals(form.bind().errors, Nil)
    assertEquals(form.bind("phoneNo" -> "").errors, Nil)
  }
  
}
