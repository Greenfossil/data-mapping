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

class ProductMappingPathAccessSuite extends munit.FunSuite {
  import Mapping.*

  test("2 level nested named tuple") {
    val tf: Mapping[(String, (Int, String))] = tuple(
      "f1" -> text,
      "t2" -> tuple(
        "t3" -> number,
        "f2" -> text
      )
    ).name("t1")

    val t3 = tf("t1.t2.t3")
    assertNoDiff(t3.name, "t1.t2.t3")

  }

  test("2 level nested named tuple, parent child with same name") {
    val tf: Mapping[(String, (Int, String))] = tuple(
      "f1" -> text,
      "t1" -> tuple(
        "t1" -> number,
        "f2" -> text
      )
    ).name("t1")

    val t3_1 = tf("t1") //Root mapping
    val t3_2 = t3_1("t1") //Level 2 mapping
    val t3_3 = t3_2("t1") //Level 1 mapping

    assertNoDiff(t3_1.name, "t1")
    assertNoDiff(t3_2.name, "t1.t1")
    assertNoDiff(t3_3.name, "t1.t1.t1")
    assertNoDiff(t3_3.tpe, "Int")
    assertNoDiff(t3_3.tpe, tf("t1.t1.t1").tpe)
  }

  test("2 level nested anonymous tuple, parent child with same name") {
    val tf: Mapping[(String, (Int, String))] = tuple(
      "f1" -> text,
      "t1" -> tuple(
        "t1" -> number,
        "f2" -> text
      )
    )

    val t3_2 = tf("t1") //Level 2
    val t3_3 = t3_2("t1") //Level 3

    assertNoDiff(t3_2.name, "t1")
    assertNoDiff(t3_3.name, "t1.t1")
    assertNoDiff(t3_3.tpe, "Int")
    assertNoDiff(t3_3.tpe, tf("t1.t1").tpe)
  }

  test("fill  2 level nesting field") {
    case class Name(firstname: String, lastname: String)
    case class User(name: Name, addr: Int)

    val form: Mapping[User] = Mapping(
      "user", mapping[User](
        "name" -> mapping[Name](
          "firstname" -> text,
          "lastname" -> text
        ),
        "addr" -> number
      )
    )

    val filledForm = form.fill(User(Name("homer", "simpson"), 123456))

    assertEquals(filledForm("user")(0)("name")("firstname").typedValueOpt, Option("homer"))
    assertEquals(filledForm("user")("name")("firstname").bindingValueOpt, Option("homer"))
    assertEquals(filledForm("user.name.firstname").typedValueOpt, filledForm("user")("name")("firstname").typedValueOpt)
    assertEquals(filledForm("user.name.firstname").bindingValueOpt, filledForm("user")("name")("firstname").bindingValueOpt)

    val filledForm2 = form.fill(null)
    assertEquals(filledForm2("user")("name")("firstname").typedValueOpt, None)
    assertEquals(filledForm2("user")("name")("firstname").bindingValueOpt, None)
    assertEquals(filledForm2("user.name.firstname").typedValueOpt, filledForm2("user")("name")("firstname").typedValueOpt)
    assertEquals(filledForm2("user.name.firstname").bindingValueOpt, filledForm2("user")("name")("firstname").bindingValueOpt)
  }

  test("accessing inner value of repeated tuple") {
    val form: Mapping[(Option[String], Seq[(Option[String], Option[String])])] = tuple(
      "parent" -> optional(text),
      "children" -> repeatedTuple(
        "firstname" -> optional(text),
        "lastname" -> optional(text)
      ).transform(
        tup => tup,
        tup => tup
      )
    )

    val boundForm = form.bind("children[0].firstname" -> "Bart", "children[0].lastname" -> "Simpson", "children[2].firstname" -> "Lisa")
    val bart = boundForm("children")(0)("firstname").typedValueOpt
    val lisa = boundForm("children")(1)("firstname").typedValueOpt //Note: index is different from the binding value

    assertEquals[Any, Any](bart, Option(Option("Bart")))
    assertEquals[Any, Any](lisa, Option(Option("Lisa")))

    val a = boundForm("children")
    val b = boundForm("children[0]")
    val c = boundForm("children[0].firstname")

    assertEquals(boundForm("children[0].firstname").typedValueOpt, boundForm("children")(0)("firstname").typedValueOpt)
    assertEquals(boundForm("children[0].firstname").errors, boundForm("children")(0)("firstname").errors)
    assertEquals(boundForm("children[0]").typedValueOpt, Option((Some("Bart"), Some("Simpson"))))

    assertEquals(form("children[0]").typedValueOpt, None)

  }

  test("Transformation fill - Option[Name]") {
    case class Name(firstname: String, surname: String)

    val form: Mapping[Option[Name]] = Mapping(
      "name", tuple(
        "firstname" -> optional(text),
        "surname" -> optional(text).bindName("lastname")
      )
    ).transform[Option[Name]](
      t => if t(0).isEmpty && t(1).isEmpty then None else Some(Name(t(0).orNull, t(1).orNull)),
      nameOpt => (nameOpt.map(_.firstname), nameOpt.map(_.surname))
    )

    val filledForm = form.fill(Some(Name("homer", "simpson")))

    assertEquals(filledForm("name")("firstname").typedValueOpt, Option(Option("homer")))
    assertEquals(filledForm("name")("firstname").bindingValueOpt, Option("homer"))
    assertEquals(filledForm("name.firstname").typedValueOpt, filledForm("name")("firstname").typedValueOpt)
    assertEquals(filledForm("name.firstname").bindingValueOpt, filledForm("name")("firstname").bindingValueOpt)
    assertEquals(filledForm("name.firstname").name, "name.firstname")
    assertEquals(filledForm("name.lastname").bindNameOpt, Option("name.lastname"))

    val filledForm2 = form.fill(None)
    assertEquals(filledForm2("name")("firstname").typedValueOpt, Some(None))
    assertEquals(filledForm2("name")("firstname").bindingValueOpt, None)
    assertEquals(filledForm2("name.firstname").typedValueOpt, filledForm2("name")("firstname").typedValueOpt)
    assertEquals(filledForm2("name.firstname").bindingValueOpt, filledForm2("name")("firstname").bindingValueOpt)
  }

  test("  fill - named tuple in anonymous tuple") {
    case class Name(firstname: String, surname: String)

    val form: Mapping[(Option[String], Option[Name])] = tuple(
      "optional" -> optional(text),
      "name" -> tuple(
        "firstname" -> optional(text),
        "surname" -> optional(text).bindName("lastname")
      ).transform[Option[Name]](
        t => if t(0).isEmpty && t(1).isEmpty then None else Some(Name(t(0).orNull, t(1).orNull)),
        nameOpt => (nameOpt.map(_.firstname), nameOpt.map(_.surname))
      )
    )

    val filledForm = form.fill(None, Some(Name("homer", "simpson")))

    assertEquals(filledForm("name")("firstname").typedValueOpt, Option(Option("homer")))
    assertEquals(filledForm("name")("firstname").bindingValueOpt, Option("homer"))
    assertEquals(filledForm("name.firstname").typedValueOpt, filledForm("name")("firstname").typedValueOpt)
    assertEquals(filledForm("name.firstname").bindingValueOpt, filledForm("name")("firstname").bindingValueOpt)
    assertEquals(filledForm("name.firstname").name, "name.firstname")
    assertEquals(filledForm("name.lastname").bindNameOpt, Option("name.lastname"))

    val filledForm2 = form.fill(None, None)
    assertEquals(filledForm2("name")("firstname").typedValueOpt, Some(None))
    assertEquals(filledForm2("name")("firstname").bindingValueOpt, None)
    assertEquals(filledForm2("name.firstname").typedValueOpt, filledForm2("name")("firstname").typedValueOpt)
    assertEquals(filledForm2("name.firstname").bindingValueOpt, filledForm2("name")("firstname").bindingValueOpt)
  }


  test("Bind field Seq[Tuple]") {
    val tupleField: Mapping[Seq[(String, Int)]] = repeatedTuple(
      "name" -> text,
      "number" -> number
    ).name("contact")

    val boundField = tupleField.bind(
      "contact[0].name" -> "Homer",
      "contact[0].number" -> "123",
      "contact[1].name" -> "Marge",
      "contact[1].number" -> "456"
    )
    val xs  = Seq(("Homer", 123), ("Marge", 456))
    assertEquals(boundField.typedValueOpt, Some(xs))

    //Access via path
    assertEquals(boundField("contact").name, "contact")
    assertEquals(boundField("contact").typedValueOpt, Some(Seq(("Homer", 123), ("Marge", 456))))
    assertEquals(boundField("contact").errors, Nil)

    assertEquals(boundField("contact[0]").typedValueOpt, Some(("Homer", 123)))
    assertEquals(boundField("contact[0]").errors, Nil)

    assertEquals(boundField("contact[0].name").name, "contact.name")
    assertEquals(boundField("contact[0].name").typedValueOpt, Option("Homer"))
    assertEquals(boundField("contact[0].name").errors, Nil)
    assertEquals(boundField("contact[0].name").bindingValueOpt, Option("Homer"))

    //Access via field
    assertEquals(boundField("contact")(0).typedValueOpt, Some(("Homer", 123)))
    assertEquals(boundField("contact")(0).errors, Nil)

    assertEquals(boundField("contact")(0)("name").name, "contact.name")
    assertEquals(boundField("contact")(0)("name").bindingName, "contact[0].name")
    assertEquals(boundField("contact")(0)("name").typedValueOpt, Option("Homer"))
    assertEquals(boundField("contact")(0)("name").errors, Nil)
    assertEquals(boundField("contact")(0)("name").bindingValueOpt, Option("Homer"))

    //Access via boundFields
    val indexedBound = boundField.asInstanceOf[SeqMapping[(String, Int)]]
      .boundFieldsWithPadding(0)([A] => (mapping:Mapping[A], index:Int) => mapping)

    assertNoDiff(indexedBound(0).name, "contact")
    assertNoDiff(indexedBound(0)("name").name, "contact.name")
    assertNoDiff(indexedBound(0)("name").bindingName, "contact[0].name")
    assertNoDiff(indexedBound(0)("number").name, "contact.number")
    assertNoDiff(indexedBound(0)("number").bindingName, "contact[0].number")
    assertEquals(indexedBound(0).typedValueOpt, Some(("Homer", 123)))
    assertNoDiff(indexedBound(1).name, "contact")
    assertNoDiff(indexedBound(1)("name").name, "contact.name")
    assertNoDiff(indexedBound(1)("name").bindingName, "contact[1].name")
    assertNoDiff(indexedBound(1)("number").name, "contact.number")
    assertNoDiff(indexedBound(1)("number").bindingName, "contact[1].number")
    assertEquals(indexedBound(1).typedValueOpt, Some(("Marge", 456)))

  }

  test(" 2 level nested unnamed tuple") {
    val tf: Mapping[(String, (Int, String))] = tuple(
      "f1" -> text,
      "t2" -> tuple(
        "t3" -> number,
        "f2" -> text
      )
    )

    val t3 = tf("t2.t3")

    assertNoDiff(t3.name, "t2.t3")
    assertEquals(t3.bindNameOpt, Some("t2.t3"))
  }

  test("Tuple[Seq[Tuple]]") {
    val tupleField: Mapping[(Option[String], Seq[(String, Int)])] = tuple(
      "value" -> optional(text),
      "contact" -> repeatedTuple(
        "name" -> text,
        "number" -> number
      )
    )

    val boundField = tupleField.bind(
      "contact[0].name" -> "Homer",
      "contact[0].number" -> "123",
      "contact[1].name" -> "Marge",
      "contact[1].number" -> "456"
    )
    assertEquals(boundField.typedValueOpt, Some((None, Seq(("Homer", 123), ("Marge", 456)))))

    assertEquals(boundField("contact").typedValueOpt, Some(Seq(("Homer", 123), ("Marge", 456))))
    assertEquals(boundField("contact").errors, Nil)

    assertEquals(boundField("contact[0]").typedValueOpt, Some(("Homer", 123)))
    assertEquals(boundField("contact[0]").errors, Nil)

    assertEquals(boundField("contact[0].name").typedValueOpt, Option("Homer"))
    assertEquals(boundField("contact[0].name").errors, Nil)
    assertEquals(boundField("contact[0].name").bindingValueOpt, Option("Homer"))
  }

}
