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

case class OptionalMapping[A](tpe: String, mapping: Mapping[A],
                              typedValueOpt: Option[Option[A]] = None,
                              errors: Seq[MappingError] = Nil,
                              constraints: Seq[Constraint[Option[A]]] = Nil) extends Mapping[Option[A]]:

  override def isRequired: Boolean = false

  override val name: String = mapping.name


  override def usedChildNameOpt: Option[String] =
    mapping.usedChildNameOpt

  override def setUsedChildNameOpt(name: String): Mapping[Option[A]] =
    copy(mapping = mapping.setUsedChildNameOpt(name))

  override val bindNameOpt: Option[String] = mapping.bindNameOpt

  /**
   * Alias for method field()
   *
   * @param key
   * @tparam A
   * @return
   */
  def apply[B](key: String): Mapping[B] =
    mapping match
      case _: FieldMapping[A] => this.asInstanceOf[Mapping[B]]
      case _ => mapping.apply(key)

  def apply[A](index: Int): Mapping[A] =
    mapping.apply(index)

  override def name(name: String): OptionalMapping[A] =
    copy(mapping = mapping.name(name))

  override def bindName(bindName: String): OptionalMapping[A] =
    copy(mapping = mapping.bindName(bindName))

  override def setDefaultValue(value: Option[A]): OptionalMapping[A] =
    value.fold(this)(v => copy(mapping = mapping.setDefaultValue(v)))


  override def setBindingPredicate(predicate: Option[String] => Boolean): Mapping[Option[A]] =
    copy(mapping = mapping.setBindingPredicate(predicate))

  override def bindingValueOpt: Option[String] =
    mapping.bindingValueOpt

  override def fillAndVerify(newValueOpt: Option[A])(toVerify: Boolean): OptionalMapping[A] =
    val filledMapping = newValueOpt.fold(mapping)(newValue => mapping.fillAndVerify(newValue)(toVerify))
    val errors = if !toVerify then Nil else  applyConstraints(filledMapping.typedValueOpt)
    copy(mapping = filledMapping, typedValueOpt = Some(filledMapping.typedValueOpt), errors = errors)

  override def bindUsingPrefix(prefix: String, data: Map[String, Seq[String]]): OptionalMapping[A] =
    setAndVerifyBoundMapping(mapping.bindUsingPrefix(prefix, data))

  override def bind(prefix: String, jsValue: JsValue): OptionalMapping[A] =
    setAndVerifyBoundMapping(mapping.bind(prefix, jsValue))

  private def setAndVerifyBoundMapping(boundMapping: Mapping[A]): OptionalMapping[A] =
    val boundValue = boundMapping.typedValueOpt match
      case Some(p: Product) =>
        //if a Product has all fields as None, then the boundValue will be None
        if p.productIterator.forall {
          case None => true
          case "" => true
          case Some("") => true
          case _ => false
        } then None else Some(p)
      case Some("") => None
      case Some(other) => Some(other)
      case None => None
    val errors =
      if mapping.hasAnyConstraints(Constraints.REQUIRED) && boundValue.isEmpty  then Nil
      else applyConstraints(boundValue)
    val boundErrors =
    //if boundValue isDefined then retain boundMapping.error else filter out errors in the Mapping.fields
      if boundValue.isDefined then boundMapping.errors
      else
        boundMapping.errors.flatMap { e =>
          //if boundField.bindingValue is nonEmpty then retain error else remove BinderErrors
          val nonEmptyValue = Option(boundMapping(e.key)).exists(_.bindingValueOpt.exists(_.nonEmpty))
          if nonEmptyValue then Option(e)
          else MappingError.discardMessages(e, MappingError.DiscardOptionalBinderErrors)
        }
    copy(mapping = boundMapping, typedValueOpt = Option(boundValue), errors = errors ++ boundErrors)

  /**
   *
   * @return - the number of Mapping fields
   *         For FieldMapping/SeqMapping = 1,
   *         ProductMapping = N fields,
   *         TransformMapping - depends on the underlying Mapping[A]
   */
  override def noOfFields: Int =
    mapping.noOfFields

  /**
   *
   * @return - number of value bound to Mapping
   *         For FieldMapping - min 0, max 1
   *         ProductMapping - min 0, max 1
   *         SeqMapping - min 0, max N - number of elements in the Seq
   *         TransformMapping - depends on the underlying Mapping[A]
   */
  override def boundValueIndexes: Seq[Int] =
    mapping.boundValueIndexes

  /**
   *
   * @param index - the index for the bound value
   * @return None or Option[A]
   *
   */
  override def boundValueOf(index: Int): Option[?] =
    mapping.boundValueOf(index)

  /**
   * Adds an error to this form
   *
   * @param error FormError
   */
  override def withError(error: MappingError): OptionalMapping[A] =
    copy(mapping = mapping.withError(error))

  override def discardingErrors: OptionalMapping[A] =
    copy(mapping = mapping.discardingErrors)

  override def filterErrors(predicate: MappingError => Boolean): Mapping[Option[A]] = copy(mapping = mapping.filterErrors(predicate))


  override def verifying(newConstraints: Constraint[Option[A]]*): OptionalMapping[A] =
    copy(constraints = constraints ++ newConstraints)

  override def removeConstraints(nameRegex: String): OptionalMapping[A] =
    copy(constraints = constraints.filter(c => c.name.exists(n => n.matches(nameRegex))))


