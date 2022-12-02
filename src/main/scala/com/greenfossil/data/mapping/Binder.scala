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

import java.sql
import java.sql.Timestamp
import java.time.*
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.util.UUID

trait Binder[T]:

  val tpe: String

  def toBindingValue(value: T): String

  /**
   * Binds this field, i.e. constructs a concrete value from submitted data.
   *
   * @param key the field key
   * @param data the submitted data
   * @return Either a concrete value of type T or a set of error if the binding failed.
   */
  def bind(key: String, data: Option[String]): Either[Seq[MappingError], T]

  override def toString: String = s"Binder: ${tpe}"

end Binder

/** This object defines several default formatters. */
object Binder:

  /**
   * Default formatter for the `String` type, it checks if the value is missing i.e. None.
   * 
   */
  val stringFormat: Binder[String] = new Binder[String] {
    override val tpe: String = "String"

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], String] =
      data.toRight(Seq(MappingError(key, MappingError.REQUIRED, Nil)))

    override def toBindingValue(value: String): String = value

  }

  /**
   * Default formatter for the `Char` type.
   */
  val charFormat: Binder[Char] = new Binder[Char] {
    override val tpe: String = "Char"

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], Char] =
      data
        .filter(s => s.length == 1 && s != " ")
        .map(s => Right(s.charAt(0)))
        .getOrElse(Left(Seq(MappingError(key, MappingError.REQUIRED, Nil))))

    override def toBindingValue(value: Char): String = value.toString

  }

  /**
   * Helper for formatters binders
   * @param parse Function parsing a String value into a T value, throwing an exception in case of failure
   * @param errArgs Error to set in case of parsing failure
   * @param key Key name of the field to parse
   * @param data Field data
   */
  private def parsing[T](parse: String => T, errMsg: String, errArgs: Seq[Any])(key: String, data: Option[String]): Either[Seq[MappingError], T] = 
    stringFormat
      .bind(key, data)
      .flatMap { s =>
        scala.util.control.Exception
          .allCatch[T]
          .either(parse(s))
          .left
          .map(e => Seq(MappingError(key, errMsg, errArgs)))
    }

  private def numberFormatter[T](convert: String => T, real: Boolean = false): Binder[T] = {
    val errorString = if real then MappingError.REAL else MappingError.NUMBER

    new Binder[T] {
      override val tpe: String = "Number"

      override def bind(key: String, data: Option[String]): Either[Seq[MappingError], T] =
        parsing(convert, errorString, Nil)(key, data)

      override def toBindingValue(value: T): String = value.toString

    }
  }

  /**
   * Default formatter for the `Long` type.
   */
  val longFormat: Binder[Long] = numberFormatter(_.toLong)

  /**
   * Default formatter for the `Int` type.
   */
  val intFormat: Binder[Int] = numberFormatter(_.toInt)

  /**
   * Default formatter for the `Short` type.
   */
  val shortFormat: Binder[Short] = numberFormatter(_.toShort)

  /**
   * Default formatter for the `Byte` type.
   */
  val byteFormat: Binder[Byte] = numberFormatter(_.toByte)

  /**
   * Default formatter for the `Float` type.
   */
  val floatFormat: Binder[Float] = numberFormatter(_.toFloat, real = true)

  /**
   * Default formatter for the `Double` type.
   */
  val doubleFormat: Binder[Double] = numberFormatter(_.toDouble, real = true)

  /**
   * Default formatter for the `BigDecimal` type.
   */
  def bigDecimalFormat(precision: Option[(Int, Int)]): Binder[BigDecimal] = new Binder[BigDecimal] {
    override val tpe: String = "BigDecimal"

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], BigDecimal] =
      stringFormat
        .bind(key, data)
        .flatMap { s =>
          scala.util.control.Exception
            .allCatch[BigDecimal]
            .either {
              val bd = BigDecimal(s)
              precision
                .map{
                  case (p, s) =>
                    if (bd.precision - bd.scale > p - s) {
                      throw new java.lang.ArithmeticException("Invalid precision")
                    }
                    bd.setScale(s)
                }
                .getOrElse(bd)
            }
            .left
            .map { e =>
              Seq(
                precision match 
                  case Some((p, s)) => MappingError(key, MappingError.REAL_PRECISION, Seq(p, s))
                  case None         => MappingError(key, MappingError.REAL, Nil)
              )
            }
        }

    override def toBindingValue(value: BigDecimal): String = value.toString

  }

  /**
   * Default formatter for the `BigDecimal` type with no precision
   */
  val bigDecimalFormat: Binder[BigDecimal] = bigDecimalFormat(None)

  /**
   * Default formatter for the `Boolean` type.
   */
  val booleanFormat: Binder[Boolean] = new Binder[Boolean] {

    override val tpe: String = "Boolean"

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], Boolean] =
      data.getOrElse("false") match
        case "" => Right(false)
        case "true"  => Right(true)
        case "false" => Right(false)
        case _       => Left(Seq(MappingError(key, MappingError.BOOLEAN, Nil)))

    override def toBindingValue(value: Boolean): String = value.toString
  }

  import java.util.{Date, TimeZone}

  /**
   * Formatter for the `java.util.Date` type.
   *
   * @param pattern a date pattern, as specified in `format.DateTimeFormatter`.
   * @param timeZone the `java.util.TimeZone` to use for parsing and formatting
   */
  def dateFormat(pattern: String): Binder[Date] = new Binder[Date] {
    override val tpe: String = "java.util.Date"
    
    val formatter    = new java.text.SimpleDateFormat(pattern)

    def dateParse(data: String) = formatter.parse(data)

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], Date] =
      parsing(dateParse, MappingError.DATE, Nil)(key, data)

    override def toBindingValue(value: Date): String = formatter.format(value)

  }

  /**
   * Default formatter for the `java.util.Date` type with pattern `yyyy-MM-dd`.
   */
  val dateFormat: Binder[Date] = dateFormat("yyyy-MM-dd")

  /**
   * Formatter for the `java.sql.Date` type.
   *
   * @param pattern a date pattern as specified in `DateTimeFormatter`.
   */
  def sqlDateFormat(pattern: String): Binder[java.sql.Date] = new Binder[java.sql.Date] {
    override val tpe: String = "java.sql.Date"

    private val dateBinder: Binder[LocalDate] = localDateFormat(pattern)

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], java.sql.Date] =
      dateBinder.bind(key, data).map(d => java.sql.Date.valueOf(d))

    override def toBindingValue(value: sql.Date): String = dateBinder.toBindingValue(value.toLocalDate)
  }

  /**
   * Default formatter for `java.sql.Date` type with pattern `yyyy-MM-dd`.
   */
  val sqlDateFormat: Binder[java.sql.Date] = sqlDateFormat("yyyy-MM-dd")

  /**
   * Formatter for the `java.sql.Timestamp` type.
   *
   * @param pattern a date pattern as specified in `DateTimeFormatter`.
   * @param timeZone the `java.util.TimeZone` to use for parsing and formatting
   */
  def sqlTimestampFormat(pattern: String, timeZone: TimeZone = TimeZone.getDefault): Binder[java.sql.Timestamp] =
    new Binder[java.sql.Timestamp] {

      override val tpe: String = "java.sql.Timestamp"

      private val formatter = DateTimeFormatter.ofPattern(pattern).withZone(timeZone.toZoneId)

      private def timestampParse(data: String) =
        if pattern.isEmpty
        then java.sql.Timestamp.valueOf(data)
        else java.sql.Timestamp.valueOf(LocalDateTime.parse(data, formatter))

      override def bind(key: String, data: Option[String]): Either[Seq[MappingError], java.sql.Timestamp] =
        parsing(timestampParse, MappingError.TIMESTAMP, Nil)(key, data)

      override def toBindingValue(value: Timestamp): String = formatter.format(value.toLocalDateTime)
    }

  /**
   * Default formatter for `java.sql.Timestamp` type with pattern `yyyy-MM-dd HH:mm:ss`.
   */
  val sqlTimestampFormat: Binder[java.sql.Timestamp] = sqlTimestampFormat("")

  /**
   * Formatter for the `LocalDate` type.
   *
   * @param pattern a date pattern as specified in `format.DateTimeFormatter`.
   */
  def localDateFormat(pattern: String): Binder[LocalDate] = new Binder[LocalDate] {
    override val tpe: String = "LocalDate"

    val formatter = DateTimeFormatter.ofPattern(pattern)

    def localDateParse(data: String) = LocalDate.parse(data, formatter)

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], LocalDate] =
      parsing(localDateParse, MappingError.DATE, Nil)(key, data)

    override def toBindingValue(value: LocalDate): String =
      formatter.format(value)
  }

  /**
   * Default formatter for `LocalDate` type with pattern `yyyy-MM-dd`.
   */
  val localDateFormat: Binder[LocalDate] = localDateFormat("yyyy-MM-dd")

  /**
   * Formatter for the `LocalDateTime` type.
   *
   * @param pattern a date pattern as specified in `format.DateTimeFormatter`.
   * @param zoneId the `ZoneId` to use for parsing and formatting
   */
  def localDateTimeFormat(pattern: String, zoneId: ZoneId = ZoneId.systemDefault()): Binder[LocalDateTime] =
    new Binder[LocalDateTime] {
      override val tpe: String = "LocalDateTime"

      val formatter =
        if pattern.isEmpty
        then DateTimeFormatter.ISO_LOCAL_DATE_TIME
        else DateTimeFormatterBuilder().parseCaseInsensitive().appendPattern(pattern).toFormatter.withZone(zoneId)

      def localDateTimeParse(data: String) = LocalDateTime.parse(data, formatter)

      override def bind(key: String, data: Option[String]): Either[Seq[MappingError], LocalDateTime] =
        parsing(localDateTimeParse, MappingError.LOCALDATETIME, Nil)(key, data)

      override def toBindingValue(value: LocalDateTime): String =
        formatter.format(value)

    }

  /**
   * Default formatter for `LocalDateTime` type with pattern `yyyy-MM-dd`.
   */
  val localDateTimeFormat: Binder[LocalDateTime] =
    localDateTimeFormat("")

  /**
   * Formatter for the `LocalTime` type.
   *
   * @param pattern a date pattern as specified in `format.DateTimeFormatter`.
   */
  def localTimeFormat(pattern: String): Binder[LocalTime] = new Binder[LocalTime] {
    override val tpe: String = "LocalTime"

    val formatter: DateTimeFormatter =
      if pattern.isEmpty
      then DateTimeFormatter.ISO_LOCAL_TIME
      else new DateTimeFormatterBuilder().parseCaseInsensitive().appendPattern(pattern).toFormatter

    def localTimeParse(data: String): LocalTime =
      LocalTime.parse(data, formatter)

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], LocalTime] =
      parsing(localTimeParse, MappingError.LOCALTIME, Nil)(key, data)

    override def toBindingValue(value: LocalTime): String =
      formatter.format(value)

  }

  /**
   * Default formatter for `LocalTime` type with pattern `HH:mm:ss`.
   */
  val localTimeFormat: Binder[LocalTime] = localTimeFormat("")

  def yearMonthFormat(pattern: String): Binder[YearMonth] = new Binder[YearMonth] {
    override val tpe: String = "YearMonth"

    val formatter = DateTimeFormatter.ofPattern(pattern)

    def yearMonthParse(data: String): YearMonth =
      if pattern.isEmpty then YearMonth.parse(data) else YearMonth.parse(data, formatter)

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], YearMonth] =
      parsing(yearMonthParse, MappingError.YEARMONTH, Nil)(key, data)

    override def toBindingValue(value: YearMonth): String =
      formatter.format(value)
  }

  val yearMonthFormat: Binder[YearMonth] = yearMonthFormat("yy-MM")

  /**
   * Default formatter for the `java.util.UUID` type.
   */
  def uuidFormat: Binder[java.util.UUID] = new Binder[java.util.UUID] {
    override val tpe: String = "java.util.UUID"

    override def bind(key: String, data: Option[String]): Either[Seq[MappingError], java.util.UUID] =
      parsing(java.util.UUID.fromString, MappingError.UUID, Nil)(key, data)

    override def toBindingValue(value: UUID): String =
      value.toString

  }
  
end Binder
