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

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime, LocalTime, YearMonth}

class MappingBind1_FieldMappingSuite extends munit.FunSuite {
  import Mapping.*

  test("string") {
    val field = text.name("field")

    val boundMissingField = field.bind()
    assertEquals(boundMissingField.errors.head.message, "error.required")
    assertEquals(boundMissingField.typedValueOpt, None)

    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField.typedValueOpt, Option(""))

    val boundValueField = field.bind("field" -> "hello world")
    assertEquals(boundValueField.errors, Nil)
    assertEquals(boundValueField.typedValueOpt, Option("hello world"))
  }

  test("int"){
    val field = number.name("field")
    assertEquals(field.boundValueIndexes, Nil)

    val boundMissingField = field.bind()
    assertEquals(boundMissingField.errors.head.message, "error.required")
    assertEquals(boundMissingField.typedValueOpt, None)

    val boundEmptyField = field.bind("field" -> "")
    assertNoDiff(boundEmptyField.errors.head.message, "error.number")
    assertEquals(boundEmptyField.typedValueOpt, None)
    assertEquals(boundEmptyField.bindingValueOpt, Option(""))

    val boundValueField = field.bind("field" -> "1")
    assertEquals(boundValueField.errors, Nil)
    assertEquals(boundValueField.typedValueOpt, Option(1))
    assertEquals(boundValueField.bindingValueOpt, Option("1"))
    assertEquals(boundValueField.boundValueIndexes, Seq(0))
    assertEquals(boundValueField.boundValueOf(0), Some(1))
  }

  test("long"){
    val field = longNumber.name("field")
    val boundField = field.bind("field" -> "200000")
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Option(200000L))
  }

  test("double"){
    val field = double.name("field")
    val boundField = field.bind("field" -> "200000")
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Option(200000D))
  }

  test("float"){
    val field = float.name("field")
    val boundField = field.bind("field" -> "200000")
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Option(200000.0F))
  }

  test("boolean"){
    val field = boolean.name("field")

    val boundMissingValue = field.bind()
    assertEquals(boundMissingValue.typedValueOpt, Some(false))
    assertEquals(boundMissingValue.errors, Nil)

    val boundEmptyValue = field.bind("field" -> "")
    assertEquals(boundEmptyValue.typedValueOpt, Some(false))
    assertEquals(boundEmptyValue.errors, Nil)

    val boundField = field.bind("field" -> "false")
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Option(false))

    val boundField2 = field.bind("field" -> "hello")
    assertEquals(boundField2.errors.head.messages.head, "error.boolean")
    assertEquals(boundField2.typedValueOpt, Some(false))
    assertEquals(boundField2.bindingValueOpt, Some("hello"))
  }

  test("boolean's binding value"){
    val field = boolean.name("bool")
    val boundMissingValueField = field.bind()
    val boundEmptyValueField = field.bind("bool" -> "")
    val boundTrueValueField = field.bind("bool" -> "true")
    val boundFalseValueField = field.bind("bool" -> "false")
    val boundBadValueField = field.bind("bool" -> "bad")

    assertEquals(boundMissingValueField.typedValueOpt, Option(false))
    assertEquals(boundMissingValueField.bindingValueOpt, None)
    assertEquals(boundEmptyValueField.typedValueOpt, Option(false))
    assertEquals(boundEmptyValueField.bindingValueOpt, Option(""))
    assertEquals(boundTrueValueField.typedValueOpt, Option(true))
    assertEquals(boundTrueValueField.bindingValueOpt, Option("true"))
    assertEquals(boundFalseValueField.typedValueOpt, Option(false))
    assertEquals(boundFalseValueField.bindingValueOpt, Option("false"))
    assertEquals(boundBadValueField.typedValueOpt, Some(false))
    assertEquals(boundBadValueField.bindingValueOpt, Option("bad"))
  }

  test("local date") {
    val now = LocalDate.now
    val field = localDate.name("field")
    val boundField = field.bind("field" -> now.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(now))
  }

  test("local time"){
    val now = LocalTime.now()
    val field = localTime.name("field")
    val boundField = field.bind("field" -> now.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(now))

    //time without millis
    val nowInSecs = now.truncatedTo(ChronoUnit.SECONDS)
    assertNotEquals(now, nowInSecs, "nowInSecs should not have millis")

    val field2 = localTime.name("field")
    val boundField2 = field2.bind("field" -> nowInSecs.toString)
    assertEquals(boundField2.errors, Nil)
    assertEquals(boundField2.typedValueOpt, Some(nowInSecs))

  }

  test("local date time"){
    //By default uses ISO_LOCAL_DATE_TIME -
    // https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#ISO_LOCAL_DATE_TIME
    val now = LocalDateTime.now()
    val field = localDateTime.name("field")
    val boundField = field.bind("field" -> now.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(now))

    val nowInSecs = LocalDateTime.now().truncatedTo(ChronoUnit.SECONDS)
    assertNotEquals(now, nowInSecs, "nowInSecs should not have millis")

    val field2 = localDateTime.name("field")
    val boundField2 = field2.bind("field" -> nowInSecs.toString)
    assertEquals(boundField2.errors, Nil)
    assertEquals(boundField2.typedValueOpt, Some(nowInSecs))
  }

  test("sql date"){
    val now: java.sql.Date = java.sql.Date.valueOf("2022-02-02")
    val field = sqlDate.name("field")
    val boundField = field.bind("field" -> now.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(now))
  }

  test("sql time stamp"){
    val milli: Long = System.currentTimeMillis()
    val now: java.sql.Timestamp = new java.sql.Timestamp(milli)

    val field = sqlTimestamp.name("field")
    val boundField = field.bind("field" -> now.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(now))
  }

  test("uuid"){
    val uuidValue = java.util.UUID.randomUUID()

    val field = uuid.name("field")
    val boundField = field.bind("field" -> uuidValue.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(uuidValue))
  }

  test("byte"){
    val value: Byte = 123.toByte
    val field = byteNumber.name("field")
    val boundField = field.bind("field" -> value.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(value))
  }

  test("short"){
    val value: Short = 1
    val field = shortNumber.name("field")
    val boundField = field.bind("field" -> value.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(value))
  }

  test("big decimal"){
    val value: BigDecimal = BigDecimal(200000)
    val field = bigDecimal.name("field")
    val boundField = field.bind("field" -> value.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(value))
  }

  test("char"){
    val value: Char = 'a'

    val field = char.name("field")
    val boundField = field.bind("field" -> value.toString)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(value))
  }

  test("year month type"){
    val data = YearMonth.now()
    val pattern = "yy-MM"
    val field: Mapping[YearMonth] = yearMonthUsing(pattern).name("field")

    val formatter = java.time.format.DateTimeFormatter.ofPattern(pattern)
    val value = formatter.format(data)
    val boundField = field.bind("field" -> value)
    assertEquals(boundField.typedValueOpt, Some(data))
    assertEquals(boundField.errors, Nil)

    val field2 = yearMonthUsing("yyyy-MM").name("field")
    val boundField2 = field2.bind("field" -> data.toString)
    assertEquals(boundField2.typedValueOpt, Some(data))
    assertEquals(boundField2.errors, Nil)

  }

  test("LocalDate with pattern field"){
    val now = LocalDate.now()
    val format = "MMMM d, yyyy"
    val nowValue = now.format(DateTimeFormatter.ofPattern(format))
    val field = localDateUsing(format).name("field")

    val boundField = field.bind("field"-> nowValue)
    assertEquals(boundField.errors, Nil)
    assertEquals(boundField.typedValueOpt, Some(now))
  }

  test("Bind error"){
    val field = number.name("field")
    val boundField = field.bind("field" -> "abc")
    assertEquals(boundField.errors.head.message, "error.number")
    assertEquals(boundField.typedValueOpt, None)
  }

}
