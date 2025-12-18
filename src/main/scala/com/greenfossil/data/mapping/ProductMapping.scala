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

case class ProductMapping[A](tpe: String,
                             name: String = null,
                             bindNameOpt: Option[String] = None,
                             typedValueOpt: Option[A] = None,
                             bindingValueOpt: Option[String] = None,
                             constraints:Seq[Constraint[A]] = Nil,
                             errors: Seq[MappingError] = Nil,
                             mappings: Mapping[?] *: Tuple = null,
                             usedChildNameOpt: Option[String] = None,
                             mirrorOpt: Option[scala.deriving.Mirror.ProductOf[A]] = None) extends Mapping[A]:

  override def name(name: String): ProductMapping[A] = copy(name = name)

  override def bindName(bindName: String): ProductMapping[A] =
    if bindName.isEmpty then this
    else copy(bindNameOpt = Option(bindName))

  override def isRequired: Boolean = true

  override def setUsedChildNameOpt(name: String): Mapping[A] =
    copy(usedChildNameOpt = Option(name))

  override def setDefaultValue(value: A): ProductMapping[A] =
    copy(typedValueOpt = Option(value))

  def mapping(name: String): Option[Mapping[?]] =
    mappings.toList.asInstanceOf[List[Mapping[?]]].find(_.name == name)

  override def setBindingPredicate(predicate: Option[String] => Boolean): Mapping[A] =
    val newMappings =
      mappings.map[[X] =>> Mapping[?]] {
        [X] => (x: X) => x match
          case f: Mapping[?] =>
            f.setBindingPredicate(predicate)
      }
    copy(mappings = newMappings)

  private def bindToProduct(prefix: String, data: Map[String, Seq[String]]): ProductMapping[A] = {
    val pathName = getPathName(prefix, bindingName)
    val newMappings =
      mappings.map[[X] =>> Mapping[?]] {
        [X] => (x: X) => x match
          case f: Mapping[?] =>
            f.bindUsingPrefix(pathName, data)
      }

    bindNewMappingsToProduct(newMappings)
  }

  override def bindUsingPrefix(prefix: String, data: Map[String, Seq[String]]): ProductMapping[A] =
    bindToProduct(prefix, data)

  override def fillAndVerify(newValue: A)(toVerify: Boolean): ProductMapping[A] =
    newValue match {
      case tuple: Tuple =>
        val iter = tuple.productIterator
        val newMappings = mappings.map[[X] =>> Mapping[?]]([X] => (t: X) => t match {
          case f: Mapping[a] => f.fillAndVerify(iter.next().asInstanceOf[a])(toVerify)
        })

        bindNewMappingsToProduct(newMappings, toVerify)

      case product: Product =>
        val iter = Tuple.fromProduct(product).productIterator
        val newMappings = mappings.map[[X] =>> Mapping[?]]([X] => (t: X) => t match {
          case f: Mapping[a] => f.fillAndVerify(iter.next().asInstanceOf[a])(toVerify)
        })
        bindNewMappingsToProduct(newMappings)

      case _ => this
    }

  override def verifying(newConstraints: Constraint[A]*): ProductMapping[A] =
    copy(constraints = constraints ++ newConstraints)

  def bind(prefix: String, jsValue: JsValue): ProductMapping[A] = {
    val value = if name == null || name == "" then jsValue else jsValue \ name
    val newMappings =
      mappings.map[[X] =>> Mapping[?]] {
        [X] => (x: X) => x match {
          case f: Mapping[?] =>
            f.bind(value)
        }
      }

    bindNewMappingsToProduct(newMappings)
  }

  private def bindNewMappingsToProduct(newMappings: Mapping[?] *: Tuple, toVerify: Boolean = true): ProductMapping[A] =
    boundFieldsToProduct(newMappings, mirrorOpt,
      (newData, newMappings, newValue, newErrors) =>
        if newData.values.forall(_ == null)
        then copy(errors = newErrors)
        else copy(mappings = newMappings, typedValueOpt = Option(newValue), errors = if toVerify then newErrors else Nil))

  private def boundFieldsToProduct(
                                    newMappings: Mapping[?] *: Tuple,
                                    mirrorOpt: Option[scala.deriving.Mirror.ProductOf[A]],
                                    fn: (Map[String, Any], Mapping[?] *: Tuple, A, Seq[MappingError]) => ProductMapping[A]
                                  ): ProductMapping[A] = {

    //Unwrap value if exists
    def safeValue(optValue: Option[?]): Any = if optValue.isDefined then optValue.get  else optValue

    val newData: Map[String, Any] = newMappings.toList.collect { case f: Mapping[?] => f.name -> safeValue(f.typedValueOpt) }.toMap

    val fieldsErrors: List[MappingError] = newMappings.toList.collect { case f: Mapping[?] => f.errors }.flatten

    val boundFieldValues: Any *: Tuple =
      //FIXED  https://github.com/scala/scala3/issues/20149
      val newMappings1: Mapping[?] *: Tuple = newMappings
      newMappings1.map[[A] =>> Any] {
        [X] => (x: X) => x match
          case f: Mapping[?] => safeValue(f.typedValueOpt)
      }

    val boundValue: A =
    // This is to handle Form with single field to return the actual type of the field [T]
      if newMappings.size == 1
      then
        boundFieldValues(0).asInstanceOf[A]
      else
        //If all values are None, implies value is null Or Any Missing Required fields
        val noneCnt = boundFieldValues.toList.collect { case None => 1 }.sum
        val missingRequiredFieldCnt = newMappings.toList.collect{ case f: Mapping[?] if f.isRequired && f.typedValueOpt.isEmpty => 1}.sum

        if fieldsErrors.nonEmpty &&  (noneCnt == newMappings.size || missingRequiredFieldCnt > 0)
        then null.asInstanceOf[A]
        else scala.util.Try(mirrorOpt.map(m => m.fromProduct(boundFieldValues)).getOrElse(boundFieldValues.asInstanceOf[A])).getOrElse(null.asInstanceOf[A])

    val formConstraintsErrors = if fieldsErrors.nonEmpty then fieldsErrors else applyConstraints(boundValue)

    fn(newData, newMappings, boundValue, formConstraintsErrors)
  }

  /**
   * Process a dot-path name if None, the attempt to process the key as name
   *
   * @param key
   * @tparam A
   * @return
   */
  override def apply[A](key: String): Mapping[A] =
    findMappingByName(this, key)
      .getOrElse {
        val ex = Exception(s"Field [${key}] does not exist, please check code")
        mappingLogger.error(ex.getMessage, ex)
        null
      }.asInstanceOf[Mapping[A]]

  def apply[A](index: Int): Mapping[A] =
    if index == 0 then this.asInstanceOf[Mapping[A]]
    else throw IndexOutOfBoundsException("FieldMapping cannot have index more than 0")

  override def removeConstraints(nameRegex: String): ProductMapping[A] =
    val newMappings = mappings.map[[A] =>> Mapping[?]](
      [A] => (a: A) => a match
        case m: Mapping[?] => m.removeConstraints(nameRegex)
    )
    copy(mappings = newMappings)

  /**
   * Adds an error to this form
   *
   * @param error FormError
   */
  override def withError(error: MappingError): ProductMapping[A] =
    copy(errors = this.errors :+ error)

  override def discardingErrors: ProductMapping[A] =
    val newMappings = mappings.map[[A] =>> Mapping[?]](
      [A] => (a: A) => a match
        case m: Mapping[?] => m.discardingErrors
    )
    
    copy(errors = Nil, mappings = newMappings)

  override def filterErrors(predicate: MappingError => Boolean): ProductMapping[A] =
    val newMappings = mappings.map[[A] =>> Mapping[?]](
      [A] => (a: A) => a match
        case m: Mapping[?] => m.filterErrors(predicate)
    )
    copy(errors = errors.filter(predicate), mappings = newMappings)


  override def noOfFields: Int =
    mappings.toList.size

  /**
   *
   * @return
   */
  override def fieldNames: Seq[String] =
    mappings.toList.collect { case f: Mapping[?] => f.name }

  /**
   *
   * @return - the field names of the Mapping
   * For FieldMapping - Seq(name)
   * ProductMapping - Seq of field names
   * SeqMapping - Seq(name)
   * TransformMapping - depends on the underlying Mapping[A]
   */
  override def fieldBindingNames: Seq[String] =
    mappings.toList.collect { case f: Mapping[?] => f.bindingName }

  override def boundValueIndexes: Seq[Int] =
    if typedValueOpt.isEmpty then Nil else Seq(0)

  override def boundValueOf(index: Int): Option[?] =
    if index == 0 then typedValueOpt else throw IndexOutOfBoundsException("Index cannot be more than 0")

