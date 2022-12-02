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

import scala.util.Try

case class TransformMapping[A, B](tpe: String,
                                  typedValueOpt: Option[B] = None,
                                  bindingValueOpt: Option[String] = None,
                                  bConstraints: Seq[Constraint[B]] = Nil,
                                  errors: Seq[MappingError] = Nil,
                                  aMapping: Mapping[A],
                                  forwardMapping: A => B,
                                  inverseMapping: B => A,
                                  mappingErrorsFilter: (Seq[MappingError], Seq[MappingError]) => Seq[MappingError] =
                                  (aErrors, bErrors) => (aErrors ++ bErrors).distinct
                                 ) extends Mapping[B] :

  override def name(name: String): TransformMapping[A, B] =
    copy(aMapping = aMapping.name(name))

  override val name: String = aMapping.name

  override def usedChildNameOpt: Option[String] =
    aMapping.usedChildNameOpt

  override def setUsedChildNameOpt(name: String): Mapping[B] =
    copy(aMapping =  aMapping.setUsedChildNameOpt(name))

  override def isRequired: Boolean = aMapping.isRequired

  override def bindName(bindName: String): TransformMapping[A, B] =
    copy(aMapping = aMapping.bindName(bindName))

  override val bindNameOpt: Option[String] =
    aMapping.bindNameOpt

  override def setDefaultValue(value: B): TransformMapping[A, B] =
    copy(typedValueOpt = Option(value), aMapping = aMapping.setDefaultValue(inverseMapping(value)))

  override def fillAndVerify(newValue:B)(toVerify: Boolean): TransformMapping[A, B] =
    val _valueOpt = Some(newValue)
    val bErrors = _valueOpt.filter(_ => toVerify).map(v => applyConstraints(v)).getOrElse(Nil)
    
    val filledAMapping = newValue match {
      case null => aMapping
      case _ => aMapping.fillAndVerify(inverseMapping(newValue))(toVerify)
    }
    val errors = mappingErrorsFilter(filledAMapping.errors, bErrors)
    val _bindingValue = filledAMapping.bindingValueOpt
    copy(typedValueOpt =  _valueOpt, aMapping = filledAMapping, errors = errors, bindingValueOpt = _bindingValue)


  override def setBindingPrediate(predicate: Option[String] => Boolean): Mapping[B] =
    copy(aMapping =  aMapping.setBindingPrediate(predicate))

  override def bindUsingPrefix(prefix: String, data: Map[String, Seq[String]]): TransformMapping[A, B] =
    bindABoundMapping(aMapping.bindUsingPrefix(prefix, data))

  override def bind(prefix: String, jsValue: JsValue): TransformMapping[A, B] =
    bindABoundMapping(aMapping.bind(prefix, jsValue))

  private def bindABoundMapping(boundAMapping: Mapping[A]): TransformMapping[A, B] =
    val _value: Option[B] = boundAMapping.typedValueOpt.map(forwardMapping)
    val bErrors = _value.map(v => applyConstraints(bConstraints, v)).getOrElse(Nil)
    val errors = mappingErrorsFilter(boundAMapping.errors, bErrors)
    val _bindingValue = boundAMapping.bindingValueOpt
    copy(typedValueOpt = _value, aMapping = boundAMapping, errors = errors, bindingValueOpt = _bindingValue)

  override def verifying(newConstraints: Constraint[B]*): TransformMapping[A, B] =
    copy(bConstraints = bConstraints ++ newConstraints)

  override val constraints: Seq[Constraint[B]] =
    aMapping.constraints.asInstanceOf[Seq[Constraint[B]]]

  override def apply[B](key: String): Mapping[B] =
    //if key is same as name, then return the 'this' as a proxy for Mapping[A]
    if key == name
    then this.asInstanceOf[Mapping[B]]
    else
      aMapping.apply(key)
        .setErrors( mappingErrorsFilter(aMapping.errors, errors) )

  def apply[B](index: Int): Mapping[B] =
    aMapping.apply(index)
      .setErrors( mappingErrorsFilter(aMapping.errors, errors) )

  override def unwrap: Mapping[?] = aMapping.unwrap

  override def removeConstraints(nameRegex: String): Mapping[B] =
    copy(aMapping = aMapping.removeConstraints(nameRegex))

  /**
   * Adds an error to this form
   *
   * @param error FormError
   */
  override def withError(error: MappingError): TransformMapping[A, B] =
    this.copy(errors = this.errors :+ error, aMapping = aMapping.withError(error))

  override def discardingErrors: TransformMapping[A, B] =
    this.copy(errors = Nil, aMapping = aMapping.discardingErrors)
  override def filterErrors(predicate: MappingError => Boolean): TransformMapping[A, B] = this.copy(errors = errors.filter(predicate), aMapping = aMapping.filterErrors(predicate))


  override def noOfFields: Int = aMapping.noOfFields

  override def boundValueIndexes: Seq[Int] =
    aMapping.boundValueIndexes

  override def boundValueOf(index: Int): Option[?] =
    aMapping.boundValueOf(index)
