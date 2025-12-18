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

class MappingBind4_SeqMappingSuite extends munit.FunSuite {
  import Mapping.*
  
  test("Seq[Int]") {
    val seqIntField = seq[Int].name("i")
    assertEquals(seqIntField.fieldBindingNames, Seq("i"))
    assertEquals(seqIntField.boundValueIndexes, Nil)
    val boundField = seqIntField.bind("i[1]" -> "1", "i[2]" -> "2", "i[3]" -> "3")
    assertEquals(boundField.typedValueOpt, Some(Seq(1, 2, 3)))
    assertEquals(boundField.boundValueIndexes.size, 3)
    assertEquals(boundField.boundValueOf(0), Some(1))
    assertEquals(boundField.boundValueOf(1), Some(2))
    assertEquals(boundField.boundValueOf(2), Some(3))
  }

  test("List[Int]"){
    val field = list[Int].name("i")
    val boundField = field.bind("i[1]" -> "1", "i[2]" -> "2", "i[3]" -> "3")
    assertEquals(boundField.typedValueOpt, Some(List(1, 2, 3)))
  }

  test("Seq[String]") {
    val seqIntField = seq[String].name("s")
    val boundField = seqIntField.bind("s[1]" -> "1", "s[2]" -> "2", "s[3]" -> "3")
    assertEquals(boundField.typedValueOpt, Some(Seq("1", "2", "3")))
  }

  test("List[String]") {
    val field = list[String].name("s")
    val boundField = field.bind("s[1]" -> "1", "s[2]" -> "2", "s[3]" -> "3")
    assertEquals(boundField.typedValueOpt, Some(List("1", "2", "3")))
  }

  test("Seq[Mapping]") {
    case class Contact(name: String, number: Int)
    val tupleField: Mapping[Seq[Contact]] = repeatedMapping[Contact](
      "name" -> text,
      "number" -> number
    ).name("contact")

    val boundField = tupleField.bind(
      "contact[1].name" -> "Homer",
      "contact[1].number" -> "123",
      "contact[2].name" -> "Marge",
      "contact[2].number" -> "456"
    )
    assertEquals(boundField.typedValueOpt, Some(Seq(Contact("Homer", 123), Contact("Marge", 456))))
  }

  test("bind with errors"){
    val seqIntField = seq[Int].name("i")
    val boundField = seqIntField.bind("i" -> "1", "i" -> "abc", "i" -> "3").asInstanceOf[SeqMapping[Int]]
    assertEquals(boundField.boundFields.size, 3)
    assertEquals(boundField.boundFields(1).errors.head.message, "error.number")
    assertEquals[Any, Any](boundField.typedValueOpt, Some(Seq(1, 3)))

  }

  test("repeated with no []") {
    val form: Mapping[Seq[Int]] = seq[Int].name("seq")

    val boundForm = form.bind(
      "seq" -> "1",
      "seq" -> "2",
      "seq" -> "3",
    )
    assertEquals(boundForm.typedValueOpt, Some(Seq(1, 2, 3)))
  }

  test("repeated with path with no []") {
    val form: Mapping[(String, Seq[String])] =
      tuple(
        "maincourse" -> text,
        "drinks" -> seq[String]
      ).name("food")

    val boundForm = form.bind(
      "food.maincourse" -> "pizza",
      "food.drinks[1]" -> "coke",
      "food.drinks[2]" -> "pepsi"
    )
    assertEquals(boundForm.typedValueOpt, Some(("pizza", Seq("coke", "pepsi"))))
  }

  test("repeated tuples") {
    val form: Mapping[Seq[(Int, Int)]] =
      repeatedTuple(
        "num1" -> number,
        "num2" -> number
      ).name("seq")


    val boundForm = form.bind(
      "seq[0].num1" -> "1",
      "seq[0].num2" -> "2",
      "seq[1].num1" -> "11",
      "seq[1].num2" -> "22",
    )
    assertEquals(boundForm.typedValueOpt, Some(Seq((1, 2), (11, 22))))
  }

  test("repeated with same index []") {
    val form: Mapping[Seq[Int]] = seq[Int].name("seq")

    val boundForm = form.bind(
      "seq[1]" -> "1",
      "seq[1]" -> "2",
      "seq[1]" -> "3",
    )
    assertEquals(boundForm.typedValueOpt, Some(Seq(1, 2, 3)))
  }

  test("repeated with empty index []") {
    val form: Mapping[Seq[Int]] = seq[Int].name("seq")

    val boundForm = form.bind(
      "seq[1]" -> "1",
      "seq[]" -> "5",
      "seq[]" -> "2",
      "seq[2]" -> "3",
    )
    assertEquals(boundForm.typedValueOpt, Some(Seq(5, 2, 1, 3)))
  }

  test("boundFieldsWithPadding with Int") {
    val xs = seq[Int].name("xs").asInstanceOf[SeqMapping[Int]]
    val boundField = xs.fill(List(0,1)).asInstanceOf[SeqMapping[Int]]
    val remappedBoundField = boundField.boundFieldsWithPadding(3)([A] => (mapping: Mapping[A], row: Int) => mapping.name( s"${mapping.bindingName}[${row}]"))
    assertEquals(remappedBoundField(0).name, "xs[0]")
    assertEquals(remappedBoundField(0).typedValueOpt, Some(0))
    assertEquals(remappedBoundField(1).name, "xs[1]")
    assertEquals(remappedBoundField(1).typedValueOpt, Some(1))
    assertEquals(remappedBoundField(2).name, "xs[2]")
    assertEquals(remappedBoundField(2).typedValueOpt, None)
  }

  test("boundFieldsWithPadding with tuple") {
    val xs = repeatedTuple("name" -> text, "age" -> number).name("tups").asInstanceOf[SeqMapping[(String, Int)]]
    val boundField = xs.fill(List(("homer", 42))).asInstanceOf[SeqMapping[(String, Int)]]
    val remappedBoundField = boundField.boundFieldsWithPadding(2)([A] => (mapping: Mapping[A], row: Int) =>
      mapping.name(s"${mapping.bindingName}[${row}]"))

    assertEquals(remappedBoundField(0)("name").name, "tups[0].name")
    assertEquals(remappedBoundField(0)("name").typedValueOpt, Some("homer"))
    assertEquals(remappedBoundField(0)("age").name, "tups[0].age")
    assertEquals(remappedBoundField(0)("age").typedValueOpt, Some(42))
    
    assertEquals(remappedBoundField(1)("age").name, "tups[1].age")
    assertEquals(remappedBoundField(1)("age").typedValueOpt, None)
    assertEquals(remappedBoundField(1)("age").name, "tups[1].age")
    assertEquals(remappedBoundField(1)("age").typedValueOpt, None)

    assertEquals(remappedBoundField.size, 2)
  }

  test("seq inside a repeatedTuple"){
    val form: Mapping[Seq[(Option[String], Seq[Long])]] = repeatedTuple(
      "name" -> optional(text),
      "topics" -> seq[Long]
    ).name("lesson")
    
    val boundForm = form.bind(
      "lesson[0].name" -> "Simpson Family",
    "lesson[0].topics[0]" -> "123",
    "lesson[0].topics[1]" -> "456")
    assertEquals(boundForm.typedValueOpt, Option(Seq((Option("Simpson Family"), Seq(123L, 456L)))))
  }

}
