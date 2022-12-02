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

import java.time.*

class MappingConstructionSuite extends munit.FunSuite{
  import Mapping.*
  
  /**
   * Check Scalar type
   */

  checkField("Text", "String", text, false)
  checkField("TextAs", "String",textAsIs, false)
  checkField("NonEmptyText", "String", nonEmptyText, true)
  checkField("Int", "Int", number, true)
  checkField("Long", "Long", longNumber, true)
  checkField("Double","Double", double, true)
  checkField("Boolean","Boolean", boolean, true)
  checkField("Float", "Float",float, true)
  checkField("LocalDateTime","LocalDateTime", localDateTime, true)
  checkField("LocalDate", "LocalDate", localDate, true)
  checkField("LocalTime", "LocalTime", localTime, true)
  checkField("YearMonth", "YearMonth", yearMonth, true)
  checkField("SqlTimestamp", "SqlTimestamp", sqlTimestamp, true)
  checkField("SqlDate", "SqlDate", sqlDate, true)
  checkField("Date", "Date", date, true)
  checkField("UUID", "UUID", uuid, true)
  checkField("Byte", "Byte", byteNumber, true)
  checkField("Short", "Short", shortNumber, true)
  checkField("BigDecimal",  "BigDecimal", bigDecimal, true)
  checkField("Char", "Char", char, true)
  checkField("Phone", "String", phone, true)
  checkField("Email", "String", email, true)
  checkField("MobilePhone","String", mobilePhone, true)
  checkField("Default",  "String", default(""), false)
  checkField("Default Int",  "Int", default(1), false)
  checkField("Default Long",  "Long", default(1L), false)
  checkField("Default Boolean",  "Boolean", default(true), false)
  checkField("Optional", "?String", optional(text), false)
  checkField("Checked","Boolean", checked(""), true)
  checkField("Filled Int", "Int", number.fill(1), true)

  /**
   * check tuple
   */
  checkTuple("Tuple2", tuple(
    "f1" -> textAsIs,
    "f2" -> number
  ), "String", "Int")

  /**
   * check mapping
   */

  case class Address(f1: String, f2: Int)
  checkMapping("Address", mapping[Address](
    "f1" -> textAsIs,
    "f2" -> number
  ), "Address", "String", "Int")

  /**
   *  check optional scalar
   */

  checkOptional("Int", optional[Int])

  /**
   * check optional tuple
   */

  val optTuple: Mapping[Option[(String, Int)]] = optionalTuple(
    "f1" -> textAsIs,
    "f2" -> number
  )
  checkOptional("P-", optTuple)

  val tupleElemField = optTuple.asInstanceOf[OptionalMapping[?]].mapping
  checkTuple("Option's delegate - Tuple", tupleElemField, "String", "Int")

  /**
   * check optional mapping
   */
  val optMapping = optionalMapping[Address](
    "f1" -> textAsIs,
    "f2" -> number
  )
  checkOptional("P+", optMapping)

  val mappingElemField = optMapping.asInstanceOf[OptionalMapping[?]].mapping
  checkMapping("Option's delegate - Product", mappingElemField,"Address", "String", "Int" )

  /**
   * Check Seq of scalar
   */
  checkSeq("Int", seq[Int])
  checkSeq("String", seq[String]) //String will be using transformation which is '#'

  /**
   * Check Repeated tuple
   */
  val repTuple = repeatedTuple(
    "name" -> textAsIs,
    "number" -> number
  )
  checkSeq("P-",repTuple)
  val repTupElem = repTuple.asInstanceOf[SeqMapping[?]].elemField
  checkTuple("SeqMapping's element - Tuple", repTupElem, "String", "Int")

  /**
   * Check Repeated mapping
   */

  val repMapping = repeatedMapping[Address](
    "f1" -> textAsIs,
    "f2" -> number
  )
  checkSeq("P+", repMapping)
  val repMappingElem = repMapping.asInstanceOf[SeqMapping[?]].elemField
  checkMapping("Address", repMappingElem, "Address", "String", "Int")



  //--------------------------

  def checkField(name: String, tpeName: String, f: Mapping[?], isRequired: Boolean)(using munit.Location): Unit =
    test(name){
      assertNoDiff(f.tpe, tpeName)
      assertEquals(f.isRequired, isRequired)
    }

  def checkTuple(name: String, f: Mapping[?], typeList: String*)(using munit.Location): Unit =
    test(s"Tuple[${name}]"){
      assert(f.isInstanceOf[ProductMapping[?]], s"Field is not an TupleField $name")
      val prodF = f.asInstanceOf[ProductMapping[?]]
      assertNoDiff(prodF.tpe, "P-")
      assert(prodF.mirrorOpt.isEmpty)
      val mappings: List[Mapping[?]] = prodF.mappings.toList.asInstanceOf[List[Mapping[?]]]
      assertEquals(mappings.size, typeList.size)
      mappings.zip(typeList) foreach{case (mapping, t) =>
        assertNoDiff(mapping.tpe, t)
      }
    }

  def checkMapping(name: String, f: Mapping[?], className: String, typeList: String*)(using munit.Location): Unit =
    test(s"Mapping[${name}]"){
      assert(f.isInstanceOf[ProductMapping[?]], s"Field is not an MappingField $name")
      val prodF = f.asInstanceOf[ProductMapping[?]]
      assert(prodF.tpe.startsWith("P+"))
      assert(prodF.mirrorOpt.isDefined)
      assertNoDiff(prodF.mirrorOpt.get.toString, className)
      val mappings: List[Mapping[?]] = prodF.mappings.toList.asInstanceOf[List[Mapping[?]]]
      assertEquals(mappings.size, typeList.size)
      mappings.zip(typeList) foreach{case (mapping, t) =>
        assertNoDiff(mapping.tpe, t)
      }
    }

  def checkOptional[A](elemTypeName: String, f: Mapping[A])(using munit.Location): Unit =
    test(s"Option[${elemTypeName}]"){
      val optF = f.asInstanceOf[OptionalMapping[?]]
      assertNoDiff(optF.tpe, "?"+elemTypeName)
      assertNoDiff(optF.mapping.tpe, elemTypeName)
    }

  def checkSeq(elemTypeName: String, f: Mapping[?])(using munit.Location): Unit =
    test(s"Seq[$elemTypeName]"){
      assert(f.isInstanceOf[SeqMapping[?]], s"Field is not a SeqField $elemTypeName")
      val seqF = f.asInstanceOf[SeqMapping[?]]
      assertNoDiff(seqF.tpe, "[Seq")
      assertNoDiff(seqF.elemField.tpe, elemTypeName)
    }

  def checkRepeatedTuple(elemTypeName: String, f: Mapping[?])(using munit.Location): Unit =
    test(s"Seq[$elemTypeName]"){
      assert(f.isInstanceOf[SeqMapping[?]], s"Field is not a SeqField $elemTypeName")
      val seqF = f.asInstanceOf[SeqMapping[?]]
      assertNoDiff(seqF.tpe, "[")
      assertNoDiff(seqF.elemField.tpe, elemTypeName)
    }

}
