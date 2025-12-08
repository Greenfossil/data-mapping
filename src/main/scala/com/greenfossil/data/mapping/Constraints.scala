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

import com.greenfossil.data.mapping
import com.typesafe.config.ConfigFactory

import scala.util.matching.Regex

/**
 * Defines a set of built-in constraints.
 */
object Constraints extends Constraints:

  val REQUIRED = "constraint.required"
  val EMAIL = "constraint.email"
  val PHONE = "constraint.phone"
  val MOBILE = "constraint.mobile"
  val MIN = "constraint.min"
  val MAX = "constraint.max"
  val MIN_LENGTH = "constraint.minLength"
  val MAX_LENGTH = "constraint.maxLength"
  val PATTERN = "constraint.pattern"
  val PRECISION = "constraint.precision"

end Constraints

/**
 * Defines a set of built-in constraints.
 *
 * @define emailAddressDoc Defines an ‘emailAddress’ constraint for `String` values which will validate email addresses.
 *
 * '''name'''[constraint.email]
 * '''error'''[error.email]
 *
 * @define nonEmptyDoc Defines a ‘required’ constraint for `String` values, i.e. one in which empty strings are invalid.
 *
 * '''name'''[constraint.required]
 * '''error'''[error.required]
 */
trait Constraints:

  private lazy val config = ConfigFactory.load(getClass.getClassLoader)

  private lazy val emailRegex = new Regex(config.getString("validation.contact.email.addr.expression"))
  private lazy val phoneRegex = new Regex(config.getString("validation.contact.phone.generic.expression"))
  private lazy val mobileRegex = new Regex(config.getString("validation.contact.phone.mobile.expression"))

  /**
   * $emailAddressDoc
   */
  def emailAddress(errorMessage: String = MappingError.EMAIL): Constraint[String] = Constraint[String](Constraints.EMAIL) {
    e =>
      if e == null
      then Invalid(ValidationError(errorMessage))
      else if (e.trim.isEmpty) Invalid(ValidationError(errorMessage))
      else
        if emailRegex.matches(e) then Valid
        else Invalid(ValidationError(errorMessage))
  }

  /**
   * $emailAddressDoc
   *
   */
  def emailAddress: Constraint[String] = emailAddress()

  /**
   * $phoneNumberDoc
   */
  def phoneNumber(errorMessage: String = MappingError.PHONE): Constraint[String] = Constraint[String](Constraints.PHONE) {
    e =>
      if e == null
      then Invalid(ValidationError(errorMessage))
      else if (e.trim.isEmpty) Invalid(ValidationError(errorMessage))
      else
        if phoneRegex.matches(e) then Valid
        else Invalid(ValidationError(errorMessage))
  }

  /**
   * $phoneNumberDoc
   *
   */
  def phoneNumber: Constraint[String] = phoneNumber()

  /**
   * $mobileNumberDoc
   */
  def mobileNumber(errorMessage: String = "error.mobile"): Constraint[String] = Constraint[String](Constraints.MOBILE) {
    e =>
      if e == null
      then Invalid(ValidationError(errorMessage))
      else if (e.trim.isEmpty) Invalid(ValidationError(errorMessage))
      else
        if mobileRegex.matches(e) then Valid
        else Invalid(ValidationError(errorMessage))
  }

  /**
   * $mobileNumberDoc
   *
   */
  def mobileNumber: Constraint[String] = mobileNumber()

  /**
   * $nonEmptyDoc
   */
  def nonEmpty(errorMessage: String = MappingError.REQUIRED): Constraint[String] =
    Constraint[String](Constraints.REQUIRED) { o =>
      if o == null
      then Invalid(ValidationError(errorMessage))
      else if (o.trim.isEmpty) Invalid(ValidationError(errorMessage))
      else Valid
    }

  // Validates against XSS attack patterns
  def xssConstraint(errorMessage: String = MappingError.XSS_DETECTED): Constraint[String] =
    Constraint[String]("constraint.xss") { o =>
      if o == null then Valid
      else if Mapping.HTMLSanitizePat.findFirstIn(o).isDefined then Invalid(ValidationError(errorMessage))
      else Valid
    }

  /**
   * $nonEmptyDoc
   *
   */
  def nonEmpty: Constraint[String] = nonEmpty()

  /**
   * Defines a minimum value for `Ordered` values, by default the value must be greater than or equal to the constraint parameter
   *
   * '''name'''[constraint.min(minValue)]
   * '''error'''[error.min(minValue)] or [error.min.strict(minValue)]
   */
  def min[T](
              minValue: T,
              strict: Boolean = false,
              errorMessage: String = "error.min",
              strictErrorMessage: String = "error.min.strict"
            )(using ordering: scala.math.Ordering[T]): Constraint[T] = Constraint[T](Constraints.MIN, minValue) { o =>
    (ordering.compare(o, minValue).sign, strict) match
      case (1, _) | (0, false) => Valid
      case (_, false)          => Invalid(ValidationError(errorMessage, minValue))
      case (_, true)           => Invalid(ValidationError(strictErrorMessage, minValue))
  }

  /**
   * Defines a maximum value for `Ordered` values, by default the value must be less than or equal to the constraint parameter
   *
   * '''name'''[constraint.max(maxValue)]
   * '''error'''[error.max(maxValue)] or [error.max.strict(maxValue)]
   */
  def max[T](
              maxValue: T,
              strict: Boolean = false,
              errorMessage: String = "error.max",
              strictErrorMessage: String = "error.max.strict"
            )(using ordering: scala.math.Ordering[T]): Constraint[T] = Constraint[T](Constraints.MAX, maxValue) { o =>
    (ordering.compare(o, maxValue).sign, strict) match
      case (-1, _) | (0, false) => Valid
      case (_, false)           => Invalid(ValidationError(errorMessage, maxValue))
      case (_, true)            => Invalid(ValidationError(strictErrorMessage, maxValue))
  }

  /**
   * Defines a minimum length constraint for `String` values, i.e. the string’s length must be greater than or equal to the constraint parameter
   *
   * '''name'''[constraint.minLength(length)]
   * '''error'''[error.minLength(length)]
   */
  def minLength(length: Int, errorMessage: String = "error.minLength"): Constraint[String] =
    Constraint[String](Constraints.MIN_LENGTH, length) { o =>
      require(length >= 0, "string minLength must not be negative")
      if o == null
      then Invalid(ValidationError(errorMessage, length))
      else if (o.size >= length) Valid
      else Invalid(ValidationError(errorMessage, length))
    }

  /**
   * Defines a maximum length constraint for `String` values, i.e. the string’s length must be less than or equal to the constraint parameter
   *
   * '''name'''[constraint.maxLength(length)]
   * '''error'''[error.maxLength(length)]
   */
  def maxLength(length: Int, errorMessage: String = "error.maxLength"): Constraint[String] =
    Constraint[String](Constraints.MAX_LENGTH, length) { o =>
      require(length >= 0, "string maxLength must not be negative")
      if o == null
      then Invalid(ValidationError(errorMessage, length))
      else if (o.size <= length) Valid
      else Invalid(ValidationError(errorMessage, length))
    }

  /**
   * Defines a regular expression constraint for `String` values, i.e. the string must match the regular expression pattern
   *
   * '''name'''[constraint.pattern(regex)] or defined by the name parameter.
   * '''error'''[error.pattern(regex)] or defined by the error parameter.
   */
  def pattern(
               regex: => scala.util.matching.Regex,
               name: String = Constraints.PATTERN,
               error: String = "error.pattern"
             ): Constraint[String] = Constraint[String](name, () => regex) { o =>
    require(regex != null, "regex must not be null")
    require(name != null, "name must not be null")
    require(error != null, "error must not be null")

    if o == null
    then Invalid(ValidationError(error, regex))
    else regex.unapplySeq(o).map(_ => Valid).getOrElse(Invalid(ValidationError(error, regex)))
  }


  /**
   * Checks the precision and scale of the given BigDecimal
   * https://stackoverflow.com/questions/35435691/bigdecimal-precision-and-scale
   */
  def precision(
                 precision: Int,
                 scale: Int,
                 name: String = Constraints.PRECISION,
                 error: String = "error.real.precision",
               ) :Constraint[BigDecimal] = Constraint[BigDecimal](name, (precision, scale)) { bd =>
    if bd.precision - bd.scale > precision - scale
    then Invalid(ValidationError(error, precision, scale))
    else Valid
  }

end Constraints

