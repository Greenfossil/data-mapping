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

import com.greenfossil.data.mapping.Mapping
import com.greenfossil.data.mapping.Mapping.*

import java.time.LocalDateTime



class LargeTupleSuite extends munit.FunSuite {

  val largeTupleForm: Mapping[(String, String, String, String, String, String,String, String, String,String,
    String, String, String, String, String, String,String, String, String,String,
    String, String, String, String, String, String,String, String, String,String)] = Mapping.tuple(
    "1" -> text,
    "2" -> text,
    "3" -> text,
    "4" -> text,
    "5" -> text,
    "6" -> text,
    "7" -> text,
    "8" -> text,
    "9" -> text,
    "10" -> text,
    "11" -> text,
    "12" -> text,
    "13" -> text,
    "14" -> text,
    "15" -> text,
    "16" -> text,
    "17" -> text,
    "18" -> text,
    "19" -> text,
    "20" -> text,
    "21" -> text,
    "22" -> text,
    "23" -> text,
    "24" -> text,
    "25" -> text,
    "25" -> text,
    "27" -> text,
    "28" -> text,
    "29" -> text,
    "30" -> text
  )

  test("large case class"){
    assertEquals(largeTupleForm.noOfFields, 30)
  }


}
