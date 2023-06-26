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
 * A form error.
 *
 * @param key The error key (should be associated with a field.name).
 * @param messages The form message (often a simple message key needing to be translated), if more than one message
 *                 is passed the last one will be used.
 * @param args Arguments used to format the message.
 */
case class MappingError(key: String, messages: Seq[String], args: Seq[Any] = Nil):

  def this(key: String, message: String) = this(key, Seq(message), Nil)

  def this(key: String, message: String, args: Seq[Any]) = this(key, Seq(message), args)

  lazy val message: String = messages.last
  
  def is(key:String, message: String): Boolean = 
    this.key == key && this.message == message
    
  def isGlobalError: Boolean = Option(key).forall(_.isEmpty)

  /**
   * Copy this error with a new Message.
   *
   * @param message The new message.
   */
  def withMessage(message: String): MappingError = MappingError(key, message)
  
end MappingError

object MappingError:

  val REQUIRED = "error.required"
  val REAL = "error.real"
  val REAL_PRECISION = "error.real.precision"
  val NUMBER = "error.number"
  val BOOLEAN = "error.boolean"
  val DATE = "error.date"
  val TIMESTAMP = "error.timestamp"
  val LOCALDATETIME = "error.localDateTime"
  val LOCALTIME = "error.localTime"
  val YEARMONTH = "error.yearMonth"
  val UUID = "error.uuid"
  val EMAIL = "error.email"
  val PHONE = "error.phone"
  val MOBILE = "error.mobile"

  val DiscardOptionalBinderErrors: List[String] = List(REQUIRED, REAL, REAL_PRECISION, NUMBER, BOOLEAN, DATE, TIMESTAMP, LOCALDATETIME, LOCALTIME,YEARMONTH, UUID, EMAIL, PHONE, MOBILE)


  def apply(key: String, message: String) = new MappingError(key, message)

  def apply(key: String, message: String, args: Seq[Any]) = new MappingError(key, message, args)

  def discardMessages(error: MappingError, messages: Seq[String]): Option[MappingError] =
    error.messages.filterNot(m => messages.contains(m)) match {
      case Nil => None
      case xs => Option(error.copy(messages = xs))
    }

end MappingError
