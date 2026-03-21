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

class MappingConstraintsBugsSuite extends munit.FunSuite {
  import Mapping.*

  test("short number - Tests that a short number field with a minimum constraint of 0 correctly"){
    val f = shortNumber(min = 0.toShort).name("f")
    val result = f.bind("f" -> "5")
    assertEquals(result.errors.size, 0, s"Expected no errors, but got :${result.errors.map(_.message)}")
  }

  test("removeConstraints - Tests the ability to dynamically remove constraints from a field") {
    // number(min = 1, max = 100) has two constraints: constraint.min and constraint.max
    val f = number(min = 1, max = 100).name("f")

    // Verify both constraints are active: 0 fails min, 101 fails max
    assertEquals(f.bind("f" -> "0").errors.head.message, "error.min")
    assertEquals(f.bind("f" -> "101").errors.head.message, "error.max")

    // Remove the min constraint, 0 should now be valid
    val withoutMin = f.removeConstraints("constraint.min")

    assertEquals(withoutMin.bind("f" -> "0").errors.size, 0, "0 should be valid after removing min constraint")
    assertEquals(withoutMin.bind("f" -> "101").errors.size, 1, "101 should still fail max constraint")
  }
}
