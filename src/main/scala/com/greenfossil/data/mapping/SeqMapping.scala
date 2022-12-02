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

import com.greenfossil.commons.json.{JsArray, JsNull, JsObject, JsValue}

case class SeqMapping[A](tpe: String,
                         typedValueOpt: Option[Seq[A]] = None,
                         constraints:Seq[Constraint[Seq[A]]] = Nil,
                         errors: Seq[MappingError] = Nil,
                         elemField: Mapping[A],
                         minSize: Int = 0, //0 implies the list can be empty
                         boundFields: Seq[Mapping[A]] = Nil) extends Mapping[Seq[A]]:

  override def name(name: String): Mapping[Seq[A]] =
    copy(elemField = elemField.name(name))

  override def usedChildNameOpt: Option[String] =
    elemField.usedChildNameOpt

  override def setUsedChildNameOpt(name: String): Mapping[Seq[A]] =
    copy(elemField = elemField.setUsedChildNameOpt(name))

  override  val name: String = elemField.name

  override def isRequired: Boolean = minSize > 0

  override def bindingValueOpt: Option[String] = None

  override def bindName(bindName: String): Mapping[Seq[A]] =
    copy(elemField = elemField.bindName(bindName))

  override  val bindNameOpt: Option[String] = elemField.bindNameOpt

  override def setDefaultValue(values: Seq[A]): Mapping[Seq[A]] =
    values.headOption.fold(this) { v =>
      copy(elemField = elemField.setDefaultValue(v))
    }

  def boundValueIndexes: Seq[Int] =
    typedValueOpt match
      case Some(xs: Seq[?]) => xs.indices.toList
      case _ => Nil

  override def boundValueOf(index: Int): Option[?] =
    if boundFields.indices.contains(index)
    then boundFields(index).typedValueOpt
    else None


  override def setBindingPrediate(predicate: Option[String] => Boolean): Mapping[Seq[A]] =
    copy(elemField = elemField.setBindingPrediate(predicate))

  /**
   * 
   * @param minReturnSize size of for the return values and will be padded with elemMapping for the missing values
   * @param mappingConversionFn - use assign the mapping.name based on the index number 
   * @return - at least the minReturnSize size of the Seq of Mapping
   */
  def boundFieldsWithPadding(minReturnSize: Int)(mappingConversionFn: [A] => (Mapping[A], Int) => Mapping[A]): Seq[Mapping[A]] =
    val padCount = Math.max(minReturnSize - boundValueIndexes.size, 0)
    val paddedMappings = (0 until padCount).map(_ => elemField)
    (boundFields ++ paddedMappings).zipWithIndex.map{case (mapping, index) => mappingConversionFn(mapping, index)}

  override def bindUsingPrefix(prefix: String, data: Map[String, Seq[String]]): Mapping[Seq[A]] =
    bindToSeq(prefix, data)

  private def bindToSeq(prefix: String, dataMap: Map[String, Seq[String]]): Mapping[Seq[A]] = {
    val pathName = getPathName(prefix, bindingName)
      .replaceAll("\\[","\\\\[") //Need to ensure the '[' is escaped
    val keyMatchRegex = s"$pathName(\\[\\d*])?(\\..*)?"
    val keyReplaceRegex = s"($pathName(\\[(\\d*)])?)(\\.(.*))?"  // keyName is group 1, index is group 3

    /**
     * sorted according to their dataMap index if available.
     * if index is not available it will place in front
     */
    val matchedFields: Seq[(Option[Int], String)] = dataMap
      .toList
      .foldLeft(Seq.empty[(Option[Int], String)]){ (res, tup2) =>
        tup2 match
          case (key, _) if key.matches(keyMatchRegex) =>
            val indexOpt: Option[Int] = Option(key.replaceFirst(keyReplaceRegex, "$3")).filter(_.nonEmpty).flatMap(_.toIntOption)
            val name = key.replaceFirst(keyReplaceRegex, "$1") // drop the inner field names
            res :+ (indexOpt, name)
          case _ => res
      }

    //sort the matched fields before binding
    val sortedFieldNames = matchedFields.sortBy(_._1).map(_._2).distinct

    /**
     * if name is not the same key in dataMap, bind using the entire dataMap (applicable for repeatedTuples)
     * if name exists in dataMap, bind each value separately - handles multiple value to one key
     */
    val boundFields: Seq[Mapping[A]] = sortedFieldNames.flatMap{ name =>
      dataMap.getOrElse(name, Nil) match
        case Nil =>
          Seq(elemField.bindName(name).bind(dataMap))
        case values =>
          values.map{value =>
            elemField.bindName(name).bind(Map(name -> Seq(value)))
          }
    }

    val values = boundFields.collect{case f: Mapping[?] if f.typedValueOpt.isDefined => f.typedValueOpt.get}
    val errors = boundFields.collect{case f: Mapping[?] if f.errors.nonEmpty => f.errors}.flatten ++
      applyConstraints(values)

    copy(typedValueOpt = Option(values), errors = errors, boundFields = boundFields)
  }

  override def verifying(newConstraints: Constraint[Seq[A]]*): Mapping[Seq[A]] =
    copy(constraints = constraints ++ newConstraints)

  override def fillAndVerify(seqValue: Seq[A])(toVerify: Boolean): Mapping[Seq[A]] =
    seqValue match
      case xs: Seq[?] =>
        val filledFields = xs.map(x => elemField.fillAndVerify(x)(toVerify))
        val errors =
          if toVerify
          then
            filledFields.collect{case f: Mapping[?] if f.errors.nonEmpty =>
              f.errors
            }.flatten ++ applyConstraints(seqValue)
          else Nil
        copy(typedValueOpt = Option(seqValue), errors = errors, boundFields = filledFields)

  override def bind(prefix: String, jsValue: JsValue): Mapping[Seq[A]] =
    val pathName = getPathName(prefix, name)
    val data = convertJsonToDataMap(pathName, (jsValue \ pathName))
    bindToSeq(prefix, data)
    
  private def convertJsonToDataMap(path: String, json: JsValue): Map[String, Seq[String]] =
    json match
      case JsNull => Map(path -> Seq(""))
      case JsArray(elems) =>
        elems.zipWithIndex.flatMap{ case (elem, index) => convertJsonToDataMap(s"$path[$index]", elem)}.toMap
      case JsObject(dataMap) =>
        dataMap.flatMap{ case (key, jsValue) => convertJsonToDataMap(s"$path.$key", jsValue) }
      case json => Map(path -> json.asOpt[Any].map(_.toString).toList)
        

  override def apply[B](key: String): Mapping[B] =
    findMappingByName(this, key)
      .orNull
      .asInstanceOf[Mapping[B]]

  def apply[B](index: Int): Mapping[B] =
    if !boundFields.indices.contains(index) then elemField.asInstanceOf[Mapping[B]]
    else boundFields.apply(index).asInstanceOf[Mapping[B]]

  override def removeConstraints(nameRegex: String): Mapping[Seq[A]] =
    copy(elemField = elemField.removeConstraints(nameRegex))

  /**
   * Adds an error to this form
   * @param error FormError
   */
  override def withError(error: MappingError): Mapping[Seq[A]] =
    copy(errors = this.errors :+ error, boundFields = boundFields.map(_.withError(error)))

  override def discardingErrors: Mapping[Seq[A]] =
    copy(errors = Nil, boundFields = boundFields.map(_.discardingErrors))

  override def filterErrors(predicate: MappingError => Boolean): Mapping[Seq[A]] = copy(errors = errors.filter(predicate))

  override def noOfFields: Int = elemField.noOfFields
