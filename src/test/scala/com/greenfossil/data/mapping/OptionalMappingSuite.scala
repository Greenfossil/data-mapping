package com.greenfossil.data.mapping

import com.greenfossil.data.mapping.Mapping
import com.greenfossil.data.mapping.Mapping.*

class OptionalMappingSuite extends munit.FunSuite:

  test("Optional mapping with single field"){
    val optForm = Mapping("userIdOpt", optional(longNumber))
    assertEquals(optForm.isRequired, false)
    assertEquals(optForm("userIdOpt").isRequired, false)
  }

