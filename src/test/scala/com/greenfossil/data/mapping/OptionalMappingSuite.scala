package com.greenfossil.data.mapping

import com.greenfossil.data.mapping.Mapping.*

class OptionalMappingSuite extends munit.FunSuite:

  test("Optional mapping with single field"){
    val optForm: Mapping[Option[Long]] = Mapping("userIdOpt", optional(longNumber))
    assertEquals(optForm.isRequired, false)
    assertEquals(optForm("userIdOpt").isRequired, false)
  }

  test("Optional mapping with single field 2") {
    val optForm = optional(longNumber).name("userIdOpt")
    assertEquals(optForm.isRequired, false)
    assertEquals(optForm("userIdOpt").isRequired, false)
  }
