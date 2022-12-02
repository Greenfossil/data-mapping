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
 *
 * @tparam A - is the param type of Mapping
 */
trait ConstraintVerifier[A]:

  val name: String

  val constraints: Seq[Constraint[A]]
  
  def hasAnyConstraints(names: String*): Boolean =
    constraints.exists(c => c.name.exists(n => names.contains(n)))

  def verifying(newConstraints: Constraint[A]*): Mapping[A]

  def verifying(constraint: A => Boolean): Mapping[A] =
    verifying("error.unknown", constraint)

  /**
   *
   * @param error
   * @param successConstraintPredicate -  true implies no error, false implies error
   * @return
   */
  def verifying(error: String, successConstraintPredicate: A => Boolean): Mapping[A] =
    verifying(Constraint{ (a: A) =>
      if successConstraintPredicate(a) then Valid else Invalid(Seq(ValidationError(error)))
    })

  def applyConstraints(value: A): Seq[MappingError] =
    applyConstraints[A](constraints, value)

  def applyConstraints[B](constraints: Seq[Constraint[B]], value: B): Seq[MappingError] =
    constraints
      .map(_.apply(value))
      .collect { case Invalid(ve) => ve }
      .flatten
      .map(ve => MappingError(name, ve.messages, ve.args))

  def removeConstraints(nameRegex: String): Mapping[A]

end ConstraintVerifier
