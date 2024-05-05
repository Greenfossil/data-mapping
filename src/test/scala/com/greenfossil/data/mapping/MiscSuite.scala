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

class MiscSuite extends munit.FunSuite {

  import Mapping.*

  import java.time.*

  def dateListForm: Mapping[List[LocalDate]] = Mapping("dates", list(localDateUsing("dd/MM/YY")))
  def dateSeqForm: Mapping[Seq[LocalDate]] = Mapping("dates", seq(localDateUsing("dd/MM/YY")))

  test("resource-booking-mapping"){
    val filledListForm = dateListForm.fill(List(LocalDate.now))
    assertEquals(filledListForm.boundValueIndexes, List(0))
    assertEquals(filledListForm.boundValueOf(0), Option(LocalDate.now))
    val filledSeqForm = dateSeqForm.fill(Seq(LocalDate.now))
    assertEquals(filledSeqForm.boundValueIndexes, List(0))
    assertEquals(filledSeqForm.boundValueOf(0), Option(LocalDate.now))
  }

  test("LocalTime") {
    assertEquals(localTime.fill(LocalTime.of(13, 0)).typedValueOpt, Option(LocalTime.of(13, 0)))
    assertEquals(localTime.name("time").bind("time" -> "13:00").typedValueOpt, Option(LocalTime.of(13, 0)))
    assertEquals(localTimeUsing("h:mm a").name("time").bind("time" -> "1:00 pm").typedValueOpt, Option(LocalTime.of(13, 0)))
    assertEquals(localTimeUsing("h:mm a").name("time").bind("time" -> "1:00 PM").typedValueOpt, Option(LocalTime.of(13, 0)))
    assertEquals(localTimeUsing("h:mm a").name("time").bind("time" -> "1:00 pM").typedValueOpt, Option(LocalTime.of(13, 0)))

  }

}
