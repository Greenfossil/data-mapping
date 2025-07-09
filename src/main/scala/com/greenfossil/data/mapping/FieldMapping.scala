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

import com.greenfossil.commons.json.JsValue

case class FieldMapping[A](tpe: String,
                           name: String = null,
                           bindNameOpt: Option[String] = None,
                           typedValueOpt: Option[A] = None,
                           binder: Binder[A],
                           constraints:Seq[Constraint[A]] = Nil,
                           errors: Seq[MappingError] = Nil,
                           bindingPredicate: Option[String] => Boolean = BindingPredicate.Always,
                           fillValueFn: (Option[A], A) => A = ( _ /*oldValue*/: Option[A], newValue: A) => newValue,
                           bindingValuePreProcess: String => String = (a:String) => a,
                           usedChildNameOpt: Option[String] = None,
                           bindingValueOpt: Option[String] = None) extends Mapping[A]:

  override def toString: String =
    s"name:$name type:$tpe binder:${if binder != null then binder.tpe else null} value:$typedValueOpt"

  override def name(name: String): FieldMapping[A] = copy(name = name)

  override def setUsedChildNameOpt(name: String): Mapping[A] =
    copy(usedChildNameOpt = Option(name))

  override def isRequired: Boolean =
    /*
     * Non String binder without Default are required
     * Or String binder with Constraints are required
     */
    if binder == null then true
    else
      (binder.tpe != "String" && bindingPredicate != BindingPredicate.NonBlankValue)
       || (binder.tpe == "String" && hasAnyConstraints(Constraints.REQUIRED, Constraints.PHONE, Constraints.EMAIL, Constraints.MOBILE))

  override def bindName(bindName: String): FieldMapping[A] =
    if bindName.isEmpty 
    then this
    else copy(bindNameOpt = Option(bindName))

  /**
   *
   * @param value
   * @return
   */
  override def setDefaultValue(value: A): FieldMapping[A] =
    val _bindingValue =
      for {
        binder <- Option(binder)
        v <- Option(value)
      } yield binder.toBindingValue(v)
      
    copy(typedValueOpt = Some(value), bindingValueOpt = _bindingValue)

  override def fillAndVerify(newValue: A)(toVerify: Boolean): FieldMapping[A] =
    val _value: A = fillValueFn(typedValueOpt, newValue)
    val _bindingValue =
      for{
        binder <- Option(binder)
        v <- Option(_value)
      } yield binder.toBindingValue(v)

    copy(
      typedValueOpt = Some(_value),
      errors = if toVerify then applyConstraints(_value) else Nil,
      bindingValueOpt = _bindingValue
    )

  override def setBindingPredicate(predicate: Option[String] => Boolean): Mapping[A] =
    copy(bindingPredicate = predicate)

  def peekBindingValue(prefix: String, data: Map[String, Seq[String]]): Option[String] =
    val pathName = getPathName(prefix, bindingName)
    data.get(pathName).flatMap(_.headOption).map(bindingValuePreProcess)

  override def bindUsingPrefix(prefix: String, data: Map[String, Seq[String]]): FieldMapping[A] =
    val _bindingValue = peekBindingValue(prefix, data)
    if binder == null then copy(bindingValueOpt = _bindingValue)
    else
      if !bindingPredicate(_bindingValue)
      then
        val _errors = typedValueOpt.map(v => applyConstraints(v)).getOrElse(Nil)
        copy(bindingValueOpt = _bindingValue, errors = _errors)
      else
        val pathName = getPathName(prefix, bindingName)
        val boundMapping = binder.bind(pathName, _bindingValue) match
          case Left(errors) =>
            copy(errors = errors, bindingValueOpt = _bindingValue)
          case Right(value) =>
            val errors = applyConstraints(value)
            copy(typedValueOpt = Option(value), errors = errors, bindingValueOpt = _bindingValue)
        boundMapping

  override def bind(prefix: String, jsValue: JsValue): FieldMapping[A] =
    val pathName = getPathName(prefix, name)
    (jsValue \ pathName).asOpt[Any] match
      case Some(value) => bindUsingPrefix("", Map(pathName -> Seq(value.toString))) //TODO - use JsValue specific type instead
      case None => bindUsingPrefix("", Map.empty)

  override def verifying(newConstraints: Constraint[A]*): FieldMapping[A] =
    copy(constraints = constraints ++ newConstraints)

  override def removeConstraints(nameRegex: String): FieldMapping[A] =
    copy(constraints = constraints.filter(c => c.name.exists(n => n.matches(nameRegex))))

  override def apply[B](key: String): FieldMapping[B] =
    this.asInstanceOf[FieldMapping[B]]

  def apply[B](index: Int): Mapping[B] =
    if index == 0 then this.asInstanceOf[Mapping[B]]
    else throw IndexOutOfBoundsException("FieldMapping cannot have index more than 0")

  /**
   * Adds an error to this form
   * @param error FormError
   */
  override def withError(error: MappingError): FieldMapping[A] =
    this.copy(errors = this.errors :+ error)

  override def discardingErrors: FieldMapping[A] =
    this.copy(errors = Nil)

  override def filterErrors(predicate: MappingError => Boolean): FieldMapping[A] = this.copy(errors = errors.filter(predicate))


  override val noOfFields: Int = 1

  override def boundValueIndexes: Seq[Int] =
    if typedValueOpt.isEmpty then Nil else Seq(0)

  override def boundValueOf(index: Int): Option[?] =
    if index == 0 then typedValueOpt else throw IndexOutOfBoundsException("Index cannot be more than 0")


