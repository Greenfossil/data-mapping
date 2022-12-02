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
import java.time.temporal.{ChronoUnit, TemporalUnit}
import java.time.{LocalDate, LocalDateTime, LocalTime, YearMonth}

class MappingFillSuite extends munit.FunSuite {

  import Mapping.*

  test("int"){
    val field = number.name("field")
    val filledField = field.fill(1)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Option(1))
  }

  test("string"){
    val field = text.name("field")
    val filledField = field.fill("hello world")
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Option("hello world"))
  }

  test("long"){
    val field = longNumber.name("field")
    val filledField = field.fill(200000)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Option(200000L))
  }

  test("double"){
    val field = double.name("field")
    val filledField = field.fill(200000.0)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Option(200000D))
  }

  test("float"){
    val field = float.name("field")
    val filledField = field.fill(200000)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Option(200000.0F))
  }

  test("boolean"){
    val field = boolean.name("field")
    val filledField = field.fill(false)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Option(false))
  }

  test("local date") {
    val field = localDate.name("field")
    val now = LocalDate.now
    val filledField = field.fill(now)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(now))
  }

  test("local time"){
    val field = localTime.name("field")
    val now = LocalTime.now()
    val filledField = field.fill(now)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(now))

    //time without millis
    val nowInSecs = now.truncatedTo(ChronoUnit.SECONDS)
    assertNotEquals(now, nowInSecs, "nowInSecs should not have millis")

    val field2 = localTime.name("field")
    val filledField2 = field2.fill(nowInSecs)
    assertEquals(filledField2.errors, Nil)
    assertEquals(filledField2.typedValueOpt, Some(nowInSecs))

  }

  test("local date time"){
    //By default uses ISO_LOCAL_DATE_TIME -
    // https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#ISO_LOCAL_DATE_TIME
    val field = localDateTime.name("field")
    val now = LocalDateTime.now()
    val filledField = field.fill(now)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(now))

    val nowInSecs = LocalDateTime.now().truncatedTo(ChronoUnit.SECONDS)
    assertNotEquals(now, nowInSecs, "nowInSecs should not have millis")

    val field2 = localDateTime.name("field")
    val filledField2 = field2.fill(nowInSecs)
    assertEquals(filledField2.errors, Nil)
    assertEquals(filledField2.typedValueOpt, Some(nowInSecs))
  }

  test("sql date"){
    val now: java.sql.Date = java.sql.Date.valueOf("2022-02-02")
    val field = sqlDate.name("field")
    val filledField = field.fill(now)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(now))
  }

  test("sql time stamp"){
    val milli: Long = System.currentTimeMillis()
    val now: java.sql.Timestamp = new java.sql.Timestamp(milli)

    val field = sqlTimestamp.name("field")
    val filledField = field.fill(now)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(now))
  }

  test("uuid"){
    val field = uuid.name("field")

    val uuidValue = java.util.UUID.randomUUID()
    val filledField = field.fill(uuidValue)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(uuidValue))
  }

  test("byte"){
    val field = byteNumber.name("field")
    val value: Byte = 123.toByte
    val filledField = field.fill(value)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(value))
  }

  test("short"){
    val field = shortNumber.name("field")
    val value: Short = 1
    val filledField = field.fill(value)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(value))
  }

  test("big decimal"){
    val field = bigDecimal.name("field")
    val value: BigDecimal = BigDecimal(200000)
    val filledField = field.fill(value)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(value))
  }

  test("char"){
    val field = char.name("field")
    val value: Char = 'a'
    val filledField = field.fill(value)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(value))
  }

  test("year month type"){
    val pattern = "yy-MM"
    val field = yearMonthUsing(pattern).name("field")
    val data = YearMonth.now()
    val filledField = field.fill(data)
    assertEquals(filledField.typedValueOpt, Some(data))
    assertEquals(filledField.errors, Nil)

  }

  test("LocalDate with pattern field"){
    val format = "MMMM d, yyyy"
    val field = localDateUsing(format).name("field")
    val now = LocalDate.now()
    val filledField = field.fill(now)
    assertEquals(filledField.errors, Nil)
    assertEquals(filledField.typedValueOpt, Some(now))
  }

  test("seq"){
    val field: Mapping[Seq[Int]] = seq[Int].name("field")
    assertEquals(field.boundValueIndexes, Nil)
    val value = Seq(1,2,3,4,5)
    val filledField = field.fill(value)
    assertEquals(filledField.typedValueOpt, Option(value))
    assertEquals(filledField.boundValueIndexes.size, 5)
    assertEquals(filledField.boundValueOf(0),Some(1))
    assertEquals(filledField.boundValueOf(1), Some(2))
    assertEquals(filledField.boundValueOf(2), Some(3))
    assertEquals(filledField.boundValueOf(3), Some(4))
    assertEquals(filledField.boundValueOf(4), Some(5))

  }

}
