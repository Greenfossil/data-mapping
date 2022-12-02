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

import com.greenfossil.data.mapping.Mapping.*

class MappingConversionSuite extends munit.FunSuite {

  test("Convert Mapping[Seq[(String, Int)]  to Seq[Mapping[(String,Int)]]") {

    val mappingSeq: Mapping[Seq[(String,Int)]] = seq(tuple("name" -> text, "age" -> number))
    val boundMappingSeq = mappingSeq.fill(Seq(("homer", 42), ("marge", 41)))

    val e1: Mapping[(String, Int)] = boundMappingSeq(0)
    val e2: Mapping[(String, Int)] = boundMappingSeq(1)
    assertEquals(e1.typedValueOpt, Some(("homer", 42)) )
    assertEquals(e2.typedValueOpt, Some(("marge", 41)) )
  }
}
