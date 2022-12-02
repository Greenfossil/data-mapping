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

/**
 * A validation result.
 */
sealed trait ValidationResult

/**
 * Validation was a success.
 */
case object Valid extends ValidationResult

/**
 * Validation was a failure.
 *
 * @param errors the resulting errors
 */
case class Invalid(errors: Seq[ValidationError]) extends ValidationResult:

  /**
   * Combines these validation errors with another validation failure.
   *
   * @param other validation failure
   * @return a new merged `Invalid`
   */
  def ++(other: Invalid): Invalid = Invalid(this.errors ++ other.errors)

end Invalid

/**
 * This object provides helper methods to construct `Invalid` values.
 */
object Invalid:

  /**
   * Creates an `Invalid` value with a single error.
   *
   * @param error the validation error
   * @return an `Invalid` value
   */
  def apply(error: ValidationError): Invalid = Invalid(Seq(error))

  /**
   * Creates an `Invalid` value with a single error.
   *
   * @param error the validation error message
   * @param args the validation error message arguments
   * @return an `Invalid` value
   */
  def apply(error: String, args: Any*): Invalid = Invalid(Seq(ValidationError(error, args: _*)))

end Invalid


/**
 * A validation error.
 *
 * @param messages the error message, if more then one message is passed it will use the last one
 * @param args the error message arguments
 */
case class ValidationError(messages: Seq[String], args: Any*):

  lazy val message = messages.last

end ValidationError

object ValidationError:

  def apply(message: String, args: Any*) = new ValidationError(Seq(message), args: _*)

end ValidationError


