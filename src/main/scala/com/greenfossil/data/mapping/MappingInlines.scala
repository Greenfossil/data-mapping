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

import java.time.{LocalDate, LocalDateTime, LocalTime, YearMonth}

/*
* Extracts Type  't' from Mapping[t]
*/
type FieldTypeExtractor[Xs <: Tuple] <: Tuple = Xs match {
  case EmptyTuple => Xs
  case Mapping[t] *: ts => t *: FieldTypeExtractor[ts]
  case (String, Mapping[t]) *: ts => t *: FieldTypeExtractor[ts]
}

/*
 * Constructs Mapping[t] from a given type 't'
 */
type FieldConstructor[X <:Tuple] <: Tuple = X match {
  case EmptyTuple => X
  case t *: ts => Mapping[t] *: FieldConstructor[ts]
}

def toNamedFieldTuple(tuple: Tuple): Mapping[?] *: Tuple =
  tuple.map[[X] =>> Mapping[?]]([X] => (x: X) =>
    x match
      case (name: String, f: Mapping[?]) => f.name(name)
  ).asInstanceOf[Mapping[?] *: Tuple]

object BindingPredicate:

  val Value = //Bind with either Empty Value or Value
    (bindingValue: Option[String]) => bindingValue.isDefined

  val NonBlankValue = //Bind with NonEmpty Value
    //Bind only if bindingValue is not None (Missing) and notEmpty
    (bindingValue: Option[String]) => bindingValue.fold(false)(x =>  !(x == null || x.isBlank))

  val Ignore = (_: Option[String]) => false //Do not bind

  val Always = (_: Option[String]) => true //Bind even with Missing Value

end BindingPredicate

trait MappingInlines:

  import scala.compiletime.*

  inline private[mapping] def mapTo[A]: Mapping[A] =
    val x = inline erasedValue[A] match {
      case _: String             => text
      case _: Int                => number
      case _: Long               => longNumber
      case _: Double             => double
      case _: Float              => float
      case _: Boolean            => boolean
      case _: LocalDateTime      => localDateTime
      case _: LocalDate          => localDate
      case _: LocalTime          => localTime
      case _: YearMonth          => yearMonth
      case _: java.sql.Timestamp => sqlTimestamp
      case _: java.sql.Date      => sqlDate
      case _: java.util.Date     => date
      case _: java.util.UUID     => uuid
      case _: Byte               => byteNumber
      case _: Short              => shortNumber
      case _: BigDecimal         => bigDecimal
      case _: Char               => char
      case _: Option[b]          => OptionalMapping[b]("?"+transformTarget[b], mapping = mapTo[b])
      case _: Tuple              => ProductMapping("P-")
      case _: Product            => ProductMapping("P+")
      case _: Any                => FieldMapping("Any", binder = null)
    }
    x.asInstanceOf[Mapping[A]]

  inline private[mapping] def transformTarget[A]: String =
    inline erasedValue[A] match {
      case _: Option[t] => "?"+transformTarget[t]
      case _: Tuple => "P-"
      case _: Product => "P+"
      case _: String => "String"
      case _: Int => "Int"
      case _: Long => "Long"
      case _: Double => "Double"
      case _: Float => "Float"
      case _: Boolean => "Boolean"
      case _: LocalDateTime => "LocalDateTime"
      case _: LocalDate => "LocalDate"
      case _: LocalTime => "LocalTime"
      case _: YearMonth => "YearMonth"
      case _: java.sql.Timestamp => "SqlTimestamp"
      case _: java.sql.Date => "SqlDate"
      case _: java.util.Date => "Date"
      case _: java.util.UUID => "UUID"
      case _: Byte => "Byte"
      case _: Short => "Short"
      case _: BigDecimal => "BigDecimal"
      case _: Char => "Char"
      case _: Any => "Any"
    }

  /*
 * https://www.playframework.com/documentation/2.8.x/api/scala/play/api/data/Forms$.html
 */
  //Numeric
  inline def boolean: FieldMapping[Boolean] =
    FieldMapping("Boolean", binder = Binder.booleanFormat, typedValueOpt = Option(false))

  inline def byteNumber: FieldMapping[Byte] =
    FieldMapping("Byte", binder = Binder.byteFormat)

  inline def byteNumber(min: Byte = Byte.MinValue, max: Byte = Byte.MaxValue, strict: Boolean = false): Mapping[Byte] =
   byteNumber.verifying(Constraints.min(min, strict), Constraints.max(max, strict))

  inline def shortNumber: FieldMapping[Short] =
    FieldMapping("Short", binder = Binder.shortFormat)

  inline def shortNumber(min: Short = Short.MinValue, max: Short = Short.MinValue, strict: Boolean = false): Mapping[Short] =
    shortNumber.verifying(Constraints.min[Short](min, strict), Constraints.max[Short](max, strict))

  inline def number: FieldMapping[Int] =
    FieldMapping("Int", binder = Binder.intFormat)

  inline def number(min: Int, max: Int): Mapping[Int] =
    number.verifying(Constraints.min(min), Constraints.max(max))

  inline def longNumber: FieldMapping[Long] =
    FieldMapping("Long", binder = Binder.longFormat)

  inline def longNumber(min: Long = Long.MinValue, max: Long = Long.MaxValue, strict: Boolean = false): Mapping[Long] =
    longNumber.verifying(Constraints.min[Long](min, strict), Constraints.max[Long](max, strict))

  inline def double: FieldMapping[Double] =
    FieldMapping("Double", binder = Binder.doubleFormat)

  inline def float: FieldMapping[Float] =
    FieldMapping("Float", binder = Binder.floatFormat)

  inline def bigDecimal: FieldMapping[BigDecimal] =
    FieldMapping("BigDecimal", binder = Binder.bigDecimalFormat)

  inline def bigDecimal(precision: Int, scale: Int): Mapping[BigDecimal] =
    bigDecimal.verifying(Constraints.precision(precision, scale))

  //Text
  inline def char: FieldMapping[Char] =
    FieldMapping("Char", binder = Binder.charFormat)

  inline def textAsIs: FieldMapping[String] =
    text(trim = false)

  inline def text: FieldMapping[String] =
    text(trim = true)

  inline def text(trim: Boolean): FieldMapping[String] =
    val textMapping = FieldMapping("String", binder = Binder.stringFormat)
    if trim then textMapping.copy(bindingValuePreProcess = (x: String) => Option(x).map(_.trim).orNull) else textMapping

  inline def text(minLength: Int, maxLength: Int, trim: Boolean): FieldMapping[String] =
    val textMapping = text(trim)
    (minLength, maxLength)  match
      case (min, Int.MaxValue) => textMapping.verifying(Constraints.minLength(min))
      case (0, max)            => textMapping.verifying(Constraints.maxLength(max))
      case (min, max)          => textMapping.verifying(Constraints.minLength(min), Constraints.maxLength(max))

  inline def nonEmptyText: FieldMapping[String] =
    nonEmptyText(trim = true)

  inline def nonEmptyText(trim: Boolean): FieldMapping[String] =
    val textMapping = FieldMapping("String", binder = Binder.stringFormat, constraints = Seq(Constraints.nonEmpty))
    if trim then textMapping.copy(bindingValuePreProcess = _.trim) else textMapping

  inline def nonEmptyText(minLength: Int, maxLength: Int, trim: Boolean): FieldMapping[String] =
    val textMapping = FieldMapping("String", binder = Binder.stringFormat,
      constraints = Seq(Constraints.nonEmpty, Constraints.minLength(minLength), Constraints.maxLength(maxLength)))
    if trim then textMapping.copy(bindingValuePreProcess = _.trim) else textMapping

  inline def email: Mapping[String] =
    text.verifying(Constraints.emailAddress)

  inline def phone: Mapping[String] =
    text.verifying(Constraints.phoneNumber)

  inline def mobilePhone: Mapping[String] =
    text.verifying(Constraints.mobileNumber)

  //Temporal
  inline def date: FieldMapping[java.util.Date] =
    FieldMapping("Date", binder = Binder.dateFormat)

  inline def dateUsing(pattern: String): FieldMapping[java.util.Date] =
    date.copy(binder = Binder.dateFormat(pattern))

  inline def localDate: FieldMapping[LocalDate] =
    FieldMapping("LocalDate", binder = Binder.localDateFormat)

  inline def localDateUsing(pattern: String): FieldMapping[LocalDate] =
    localDate.copy(binder = Binder.localDateFormat(pattern))

  inline def localDateTime: FieldMapping[LocalDateTime] =
    FieldMapping("LocalDateTime", binder = Binder.localDateTimeFormat)

  inline def localDateTimeUsing(pattern: String): FieldMapping[LocalDateTime] =
    localDateTime.copy(binder = Binder.localDateTimeFormat(pattern))

  inline def localTime: FieldMapping[LocalTime] =
    FieldMapping("LocalTime", binder = Binder.localTimeFormat)

  inline def localTimeUsing(pattern: String): FieldMapping[LocalTime] =
    localTime.copy(binder = Binder.localTimeFormat(pattern))

  inline def yearMonth: FieldMapping[YearMonth] =
    FieldMapping("YearMonth", binder = Binder.yearMonthFormat)

  inline def yearMonthUsing(pattern: String): FieldMapping[YearMonth] =
    yearMonth.copy(binder = Binder.yearMonthFormat(pattern))

  inline def sqlDate: FieldMapping[java.sql.Date] =
    FieldMapping("SqlDate", binder = Binder.sqlDateFormat)

  inline def sqlDateUsing(pattern: String): FieldMapping[java.sql.Date] =
    sqlDate.copy(binder = Binder.sqlDateFormat(pattern))

  inline def sqlTimestamp: FieldMapping[java.sql.Timestamp] =
    FieldMapping("SqlTimestamp", binder = Binder.sqlTimestampFormat)

  inline def sqlTimestampUsing(pattern: String, timeZone: java.util.TimeZone = java.util.TimeZone.getDefault): FieldMapping[java.sql.Timestamp] =
    sqlTimestamp.copy(binder = Binder.sqlTimestampFormat(pattern, timeZone))

  inline def uuid: FieldMapping[java.util.UUID] =
    FieldMapping("UUID", binder = Binder.uuidFormat)

  inline def checked(msg: String): Mapping[Boolean] =
    boolean.verifying(msg, _ == true)

  inline def default[A](defaultValue: A): Mapping[A] =
    default(mapTo[A], defaultValue)

  def default[A](mapping: Mapping[A], defaultValue: A): Mapping[A] =
    mapping.setDefaultValue(defaultValue) match
      case f: FieldMapping[?] =>
        f.copy(
          bindingPredicate = BindingPredicate.NonBlankValue,
          fillValueFn = (oldValue: Option[A], newValue: A) =>
            //If newValue is null the fallback to value (i.e. default) else use the new value
            if newValue == null && oldValue.isDefined then oldValue.get else newValue
        )

      case m: Mapping[?] => m

  inline def ignored[A](defaultValue:A): Mapping[A] =
    ignored(mapTo[A], defaultValue)

  def ignored[A](mapping: Mapping[A], defaultValue: A): Mapping[A] =
    mapping.setDefaultValue(defaultValue) match
      case f: FieldMapping[?] =>
        f.copy(
          bindingPredicate = BindingPredicate.Ignore,
          fillValueFn = (_: Option[A], _: A) => defaultValue
        )

      case o: OptionalMapping[A] @unchecked =>
        o.mapping match
          case f: FieldMapping[A] =>
           o.copy(mapping = f.copy(
             bindingPredicate = BindingPredicate.Ignore,
             fillValueFn = (_: Option[A], _: A) => defaultValue
           ))

      case m: Mapping[?] => m

  inline def optional[A]: Mapping[Option[A]] =
    optional(mapTo[A])

  def optional[A](mapping: Mapping[A]): Mapping[Option[A]] =
    mapping match {
      case f: FieldMapping[A] =>
          OptionalMapping("?"+mapping.tpe, mapping = mapping.setBindingPrediate(BindingPredicate.Value))
      case _ : Mapping[A] =>
          OptionalMapping("?"+mapping.tpe, mapping = mapping)
    }

  inline def optionalTuple[A <: Tuple](nameValueTuple: A): Mapping[Option[FieldTypeExtractor[A]]] =
    optional(tuple[A](nameValueTuple))

  inline def optionalMapping[A <: Product](using m: scala.deriving.Mirror.ProductOf[A])
                               (nameValueTuple: Tuple.Zip[m.MirroredElemLabels, FieldConstructor[m.MirroredElemTypes]]): Mapping[Option[A]] =
    optional(mapping[A](nameValueTuple))

  inline def tuple[A <: Tuple](nameValueTuple: A): Mapping[FieldTypeExtractor[A]] =
    ProductMapping("P-", mappings = toNamedFieldTuple(nameValueTuple), mirrorOpt =  None)

  inline def mapping[A](using m: scala.deriving.Mirror.ProductOf[A])(nameValueTuple: Tuple.Zip[m.MirroredElemLabels, FieldConstructor[m.MirroredElemTypes]]): Mapping[A] =
    ProductMapping("P+", mappings = toNamedFieldTuple(nameValueTuple), mirrorOpt = Option(m))

  inline def seq[A]: Mapping[Seq[A]] =
    SeqMapping[A]("[Seq", elemField = mapTo[A]).asInstanceOf[Mapping[Seq[A]]]

  inline def seq[A](minSize: Int): Mapping[Seq[A]] =
    SeqMapping[A]("[Seq", elemField = mapTo[A], minSize = minSize).asInstanceOf[Mapping[Seq[A]]]

  inline def seq[A](mapping: Mapping[A]): Mapping[Seq[A]] =
    SeqMapping[A]("[Seq", elemField = mapping).asInstanceOf[Mapping[Seq[A]]]
  
  inline def list[A]: Mapping[List[A]] =
    SeqMapping[A]("[List", elemField = mapTo[A]).asInstanceOf[Mapping[List[A]]]

  inline def list[A](mapping: Mapping[A]): Mapping[List[A]] =
    SeqMapping[A]("[List", elemField = mapping).asInstanceOf[Mapping[List[A]]]

  inline def repeatedTuple[A <: Tuple](nameValueTuple: A): Mapping[Seq[FieldTypeExtractor[A]]] =
    SeqMapping[FieldTypeExtractor[A]](tpe = "[Seq", elemField = tuple[A](nameValueTuple))

  inline def repeatedMapping[A](using m: scala.deriving.Mirror.ProductOf[A])(nameValueTuple: Tuple.Zip[m.MirroredElemLabels, FieldConstructor[m.MirroredElemTypes]]):Mapping[Seq[A]] =
    SeqMapping[A](tpe="[Seq", elemField = mapping[A](nameValueTuple))


