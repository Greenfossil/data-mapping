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
import org.slf4j.LoggerFactory

private[mapping] val mappingLogger = LoggerFactory.getLogger("data-mapping")

object Mapping extends MappingInlines:

  inline def apply[A](name: String, mapping: Mapping[A]): Mapping[A] =
    mapping.name(name)

end Mapping


trait Mapping[A] extends ConstraintVerifier[A]:

  val tpe: String

  val name: String

  def name(name: String): Mapping[A]

  def usedChildNameOpt: Option[String]

  def setUsedChildNameOpt(name: String): Mapping[A]

  val bindNameOpt: Option[String]

  def bindName(bindName: String): Mapping[A]

  def bindingName: String =
    bindNameOpt.filterNot(_.isEmpty).getOrElse(name)

  def isRequired: Boolean 

  def isOptional: Boolean = !isRequired

  val typedValueOpt: Option[A]

  def bindingValueOpt: Option[String]

  def setDefaultValue(value: A): Mapping[A]

  def fill(newValue: A): Mapping[A] = fillAndVerify(newValue)(toVerify = false)

  def fillAndVerify(newValue: A)(toVerify: Boolean): Mapping[A]

  def setBindingPredicate(predicate: Option[String] => Boolean): Mapping[A]

  def bind(data: (String, String)*): Mapping[A] =
    bind(data.groupMap(_._1)(_._2))

  def bind(data: Map[String, Seq[String]]): Mapping[A] =
    bindUsingPrefix("", data)

  def bindUsingPrefix(prefix: String, data: Map[String, Seq[String]]): Mapping[A]

  def bind(jsValue: JsValue): Mapping[A] =
    bind("", jsValue)

  def bind(prefix: String, jsValue: JsValue): Mapping[A]

  inline def transform[B](inline forwardFn: A => B, inline inverseFn: B =>  A): TransformMapping[A, B] =
    transform(forwardFn, inverseFn, (a, b) => (a ++ b).distinct )

  inline def transform[B](forwardFn: A => B, inverseFn: B => A,
                          mappingErrorsFilter: (Seq[MappingError], Seq[MappingError]) => Seq[MappingError]): TransformMapping[A, B] =
    val tt = Mapping.transformTarget[B]
    TransformMapping[A, B](tpe = "#" + tt , aMapping = this,
      forwardMapping = forwardFn, inverseMapping = inverseFn, mappingErrorsFilter = mappingErrorsFilter)

  def unwrap: Mapping[?] = this

  /**
   *
   * @return - the number of Mapping fields
   *         For FieldMapping/SeqMapping = 1,
   *         ProductMapping = N fields,
   *         TransformMapping - depends on the underlying Mapping[A]
   */
  def noOfFields: Int

  /**
   *
   * @return
   */
  def fieldNames: Seq[String]

  /**
   *
   * @return - the field names of the Mapping
   *         For FieldMapping - Seq(name)
   *         ProductMapping - Seq of field names
   *         SeqMapping - Seq(name)
   *         TransformMapping - depends on the underlying Mapping[A]
   */
  def fieldBindingNames: Seq[String]

  /**
   *
   * @return - number of value bound to Mapping
   *         For FieldMapping - min 0, max 1
   *         ProductMapping - min 0, max 1
   *         SeqMapping - min 0, max N - number of elements in the Seq
   *         TransformMapping - depends on the underlying Mapping[A]
   */
  def boundValueIndexes: Seq[Int]

  /**
   *
   * @param index - the index for the bound value
   * @return  None or Option[A]
   *
   */
  def boundValueOf(index: Int): Option[?]

  override def toString: String =
    s"name:$name type:$tpe value:$typedValueOpt"

  private def findDotPathMapping(rootMapping: Mapping[?], key: String): Option[Mapping[?]] =
    val pathParts = key.split("\\.")
    val (_rootMapping: Seq[Mapping[?]], _pathParts: Array[String]) =
      if rootMapping.bindingName == null then (Seq(this), pathParts)
      else if rootMapping.bindingName == pathParts.head then (Seq(this), pathParts.tail)
      else (findChildMapping(rootMapping, pathParts.head).toSeq, pathParts.tail) : @unchecked

    if _rootMapping.isEmpty then None
    else
      val _mappings = _pathParts.foldLeft(_rootMapping) { (res, name) =>
        val parentMapping = res.last
        findChildMapping(parentMapping, name) match
          case Some(childMapping) => res :+ childMapping
          case None => res
      }
      val dotPath = _mappings.flatMap(m => Option(m.name)).mkString(".")
      val bindnameDotPath = _mappings.flatMap(m => Option(m.bindingName)).mkString(".")
      val mapping = _mappings.last.name(dotPath).bindName(bindnameDotPath)
      Option(mapping.asInstanceOf[Mapping[A]])

  private def findNamedMapping(mapping: Mapping[?], key: String): Option[Mapping[?]] =
    if bindingName != null && bindingName  == key && !usedChildNameOpt.contains(key)
    then Option(setUsedChildNameOpt(key))
    else {
      //search for key in the children mapping
      mapping match
        case f: FieldMapping[?] => if f.bindingName == key then Some(mapping) else None
        case m: Mapping[?] =>
          findChildMapping(m, key).map { childMapping =>
            val namePath = Seq(Option(mapping.name), Option(childMapping.name)).flatten.mkString(".")
            val bindNamePath = Seq(Option(mapping.bindingName), Option(childMapping.bindingName)).flatten.mkString(".")
            val childmapping = childMapping.name(namePath)
            val _cm = if namePath == bindNamePath then childmapping else childmapping.bindName(bindNamePath)
            //Set to use useChildMapOpt if bindingName is null
            if bindingName == null then _cm.setUsedChildNameOpt(key) else _cm
          }
    }

  private def findChildMapping(m: Mapping[?], key: String): Option[Mapping[?]] =
    val (_key, indexOpt) =
      key.split("\\[") match {
        case Array(name, indexString) =>
          (name, indexString.replaceAll("\\].*", "").toIntOption)
        case Array(name) => (name, None)
      }
    m.unwrap match
      case p: ProductMapping[?] =>
        val mappingOpt =
          p.mappings
            .toList.asInstanceOf[List[Mapping[?]]]
            .find(f => f.bindingName == _key)

        (indexOpt, mappingOpt) match
          case (Some(index), Some(mapping)) => Some(mapping.apply(index))
          case (None, Some(_)) => mappingOpt
          case _ => None

      case _: SeqMapping[?] =>
        if m.bindingName != _key then None
        else indexOpt.map(index => m.apply(index))

      case o: OptionalMapping[?] => findChildMapping(o.mapping, key)

  protected def findMappingByName(rootMapping: Mapping[?], key: String): Option[Mapping[?]] =
    if key.contains(".")
    then findDotPathMapping(rootMapping, key)
    else findNamedMapping(rootMapping, key)

  /*
   * Form APIs
   */

  /**
   * Alias for method field()
   * @param key
   * @tparam A
   * @return
   */
  def apply[B](key: String): Mapping[B]

  def apply[B](index: Int): Mapping[B]

  def fold[R](onFail: Mapping[A] => R, onSuccess: A => R): R =
    typedValueOpt match
      case Some(v) if errors.isEmpty => onSuccess(v)
      case None if errors.isEmpty => null.asInstanceOf[R]
      case _ => onFail(this)

  def errors: Seq[MappingError]
  
  def filterErrors(predicate: MappingError => Boolean): Mapping[A]
  
  def discardGlobalErrors: Mapping[A] = filterErrors(e => !e.isGlobalError)

  /**
   * Returns `true` if there is an error related to this form.
   */
  def hasErrors: Boolean = errors.nonEmpty

  /**
   * Retrieve the first error for this key.
   *
   * @param key field name.
   */
  def error(key: String): Option[MappingError] = errors.find(_.key == key)

  /**
   * Retrieve all errors for this key.
   *
   * @param key field name.
   */
  def errors(key: String): Seq[MappingError] = errors.filter(_.key == key)

  /**
   * Retrieves all global errors, i.e. errors without a key.
   *
   * @return all global errors
   */
  def globalErrors: Seq[MappingError] = errors.filter(_.isGlobalError)

  def hasGlobalErrors: Boolean = globalErrors.nonEmpty

  /**
   * Adds an error to this form
   * @param error FormError
   */
  def withError(error: MappingError): Mapping[A]

  def setErrors(errors: Seq[MappingError]): Mapping[A] =
    errors.foldLeft(discardingErrors)(_.withError(_))

  def discardingErrors: Mapping[A]

  /**
   * Adds an error to this form
   * @param key Error key
   * @param message Error message
   * @param args Error message arguments
   */
  def withError(key: String, message: String, args: Any*): Mapping[A] =
    withError(MappingError(key, message, args))

  /**
   * Adds a global error to this form
   * @param message Error message
   * @param args Error message arguments
   */
  def withGlobalError(message: String, args: String*): Mapping[A] = withError("", message, args*)

  protected def getPathName(prefix: String, name: String): String = 
    (prefix, name) match
      case (prefix, null) => prefix
      case ("", _) => name
      case (_, _) => s"$prefix.$name"

end Mapping