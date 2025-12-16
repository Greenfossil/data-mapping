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

class MappingConstraintsSuite extends munit.FunSuite {
  import Mapping.*
  
  /*
   * For implementing constraints - use play.api.data.Forms for help. Need to use Constraints class
   */

  test("nonEmptyText") {
    val f = nonEmptyText.name("f")
    val errorField = f.bind("f" -> "")
    assertEquals(errorField.errors.size, 1)
    assertEquals(errorField.errors.head.message, "error.required")

    val validField = f.bind("f" -> "abc")
    assertEquals(validField.errors, Nil)
  }

  test("email constraints") {
    val form = email.name("email")
    assertEquals(form.bind("email" -> "test email@greenfossil.com").hasErrors, true)
    assertEquals(form.bind("email" -> "test@greenfossil.com").hasErrors, false)
  }

  test("email"){
    val f = email.name("f")
    val errorField = f.bind("f" -> "https://www.google.com")
    assertEquals(errorField.errors.size, 1)
    val errorField2 = f.bind("f" -> "greenfossil.com")
    assertEquals(errorField2.errors.size, 1)
    val validField = f.bind("f" -> "test@greenfossil.com")
    assert(validField.errors.isEmpty)
  }

  test("phone"){
    val f = phone.name("f")
    assertEquals(f.bind("f" -> "12+88776655").errors.head.message, "error.phone")
    assertEquals(f.bind("f" -> "+6588776655").errors, Nil)
    assertEquals(f.bind("f" -> "6588776655").errors, Nil)
    assertEquals(f.bind("f" -> "88776655").errors, Nil)
    assertEquals(f.bind("f" -> "66776655").errors, Nil)
  }

  test("mobilePhone"){
    val f = mobilePhone.name("f")
    assertEquals(f.bind("f" -> "12+88776655").errors.head.message, "error.mobile")
    assertEquals(f.bind("f" -> "66554422").errors.head.message, "error.mobile")
    assertEquals(f.bind("f" -> "88776655").errors, Nil)
    assertEquals(f.bind("f" -> "99776655").errors, Nil)
  }

  test("number"){
    val f = number(1, 10).name("f")

    val errorField = f.bind("f" -> "20")
    assertEquals(errorField.errors.size, 1)
    val validField = f.bind("f" -> "5")

    assert(validField.errors.isEmpty)
  }

  test("text with trim option"){
    val f = text(1, 5, true).name("f")

    val validField = f.bind("f" -> "hello")
    assert(validField.errors.isEmpty)

    val errorField = f.bind("f" -> "hello world")
    assertEquals(errorField.errors.size, 1)

    val errorField2 = f.bind("f" -> "    ")
    assertEquals(errorField2.errors.size, 1)
    assertEquals(errorField2.typedValueOpt, Option(""))

    val validField2 = f.bind("f" -> "hello ")
    assert(validField2.errors.isEmpty)
    assertEquals(validField2.typedValueOpt, Option("hello"))
  }

  test("text without trim option"){
    val f = text(1, 5, false).name("f")

    val errorField = f.bind("f" -> "hello world")
    assertEquals(errorField.errors.size, 1)

    val errorField2 = f.bind("f" -> "hello ")
    assertEquals(errorField2.errors.size, 1)

    val validField = f.bind("f" -> "hello")
    assert(validField.errors.isEmpty)

    val validField2 = f.bind("f" -> "    ")
    assert(validField2.errors.isEmpty)
  }

  test("byte number"){
    val f = byteNumber(min = 2, max = 8).name("f")

    val errorField = f.bind("f" -> 10.toByte.toString)
    assertEquals(errorField.errors.size, 1)

    val validField = f.bind("f" -> "8")
    assert(validField.errors.isEmpty)
  }

  test("short number"){
    val nonStrictField = shortNumber(1,10, false).name("f")

    val nonStrictValidField = nonStrictField.bind("f" -> "1")
    assertEquals(nonStrictValidField.errors.size, 0)

    val nonStrictErrorField = nonStrictField.bind("f" -> "0")
    assertEquals(nonStrictErrorField.errors.size, 1)
    assertEquals(nonStrictErrorField.errors.head.message, "error.min")

    val strictField = shortNumber(1,10, true).name("f")
    val strictValidField = strictField.bind("f" -> "2")
    assertEquals(strictValidField.errors.size, 0)

    val strictErrorField = strictField.bind("f" -> "0")
    assertEquals(strictErrorField.errors.size, 1)
    assertEquals(strictErrorField.errors.head.message, "error.min.strict")

  }

  test("long number"){
    val nonStrictField = longNumber(1,10, false).name("f")

    val nonStrictValidField = nonStrictField.bind("f" -> "1")
    assertEquals(nonStrictValidField.errors.size, 0)

    val nonStrictErrorField = nonStrictField.bind("f" -> "0")
    assertEquals(nonStrictErrorField.errors.size, 1)
    assertEquals(nonStrictErrorField.errors.head.message, "error.min")

    val strictField = longNumber(1,10, true).name("f")
    val strictValidField = strictField.bind("f" -> "2")
    assertEquals(strictValidField.errors.size, 0)

    val strictErrorField = strictField.bind("f" -> "0")
    assertEquals(strictErrorField.errors.size, 1)
    assertEquals(strictErrorField.errors.head.message, "error.min.strict")

  }

  test("big decimal"){
    val field = bigDecimal(1,1).name("f")
    val errorField = field.bind("f" -> "1") // scale is 0, precision is 1
    assertEquals(errorField.errors.size, 1)
    assertEquals(errorField.errors.head.message, "error.real.precision")

    val validField = field.bind("f" -> "0.1") // scale is 1, precision is 1
    assertEquals(validField.errors.size, 0)

  }

  test("custom constraint"){
    val field = text
      .name("f")
      .verifying("text needs to be alphanumerical", _.matches("[a-zA-Z0-9]*"))

    val errorField = field.bind("f" -> "@#$")
    assertEquals(errorField.errors.size, 1)
    assertEquals(errorField.errors.head.message, "text needs to be alphanumerical")

    val validField = field.bind("f" -> "asdf1234")
    assert(validField.errors.isEmpty)
  }

  test("Positive XSS Constraint"){

    val errorField1 = text.name("f").bind("f" -> "<script>alert(1)</script>")
    assertEquals(errorField1.errors.size, 1)
    assertEquals(errorField1.errors.head.message, MappingError.XSS_DETECTED)

    val errorField2 = text.name("f").bind("f" -> "<script>alert(1)//.")
    assertEquals(errorField2.errors.size, 1)
    assertEquals(errorField2.errors.head.message, MappingError.XSS_DETECTED)

    val errorField3 = nonEmptyText.name("f").bind("f" -> "<script>alert(1)//.")
    assertEquals(errorField3.errors.size, 1)
    assertEquals(errorField3.errors.head.message, MappingError.XSS_DETECTED)

    assert(text.name("f").bind("f" -> "<b>safe</b>").errors.isEmpty)
    assert(nonEmptyText.name("f").bind("f" -> "<b>safe</b>").errors.isEmpty)
  }

  test("negative XSS Constraint") {

    val field1 = text.name("f").bind("f" -> "howdy")
    assertEquals(field1.errors.size, 0)

    val field2 = nonEmptyText.name("f").bind("f" -> "howdy")
    assertEquals(field2.errors.size, 0)

    val field3 = nonEmptyText.name("f").bind("f" -> "A & B")
    assertEquals(field3.errors.size, 0)

    val field4 = nonEmptyText.name("f").bind("f" -> "homer@example.com")
    assertEquals(field4.errors.size, 0)

    val field5 = nonEmptyText.name("f").bind("f" -> "1 < 2")
    assertEquals(field5.errors.size, 0)

  }

  test("sanitize false positive") {
    assertNoDiff(HtmlSanitizer.sanitize("1 < 2"), "1 &lt; 2")
  }

  test("HtmlSanitizer.isXssSafe".only) {
    // A broader set of strings that should be considered XSS-safe by the sanitizer
    val safeSamples = List(
      // plain characters and symbols
      "&",
      "<",
      ">",
      "\"",
      "'",
      "/",
      "",
      "a", "z", "A", "Z", "1", "9", "[", "]",
      // lone braces and their common variants
      "{", "{ 1", "}",
      // emails, math and simple comparisons
      "homer@example.com",
      "1 < 2",
      "1 + 1",
      "1 & 1",
      "1 / 1",
      "1\\1",
      "2 == 2",
      "true",
      "false",
      // safe HTML fragments / entities
      "<b>safe</b>",
      "<p>!@#</p>",
      "<p>1&amp;2</p>",
      "<p>&nbsp; &nbsp;sad&nbsp; &nbsp;12</p>",
      // already-escaped scripts should be safe
      "&lt;script&gt;alert(1)&lt;/script&gt;",
      // long, multi-line content (real-world use)
      """<p>Dear Team,
        |\nThe drop down values for the SGM Tree when we want to "Add A Person" is not context-sensitive to SGM roles.
        |It was once upon a time. Can help us to check? This is highlighted by user on 12 Nov.</p>""".stripMargin,
      // HTML with attributes that are benign (http/mailto) and benign data values
      "<a href=\"http://example.com\">link</a>",
      "<a href=\"mailto:foo@example.com\">email</a>",
      // text that includes angle brackets but is not HTML
      "1 < 2 and 3 > 2",
      // input with encoded entities and escapes
      "&lt;b&gt;not-bad&lt;/b&gt;",
      // CSS-like text that doesn't contain javascript
      "body { color: red }",
      // comment-only fragments
      "<!-- just a comment -->",
      // mixture of safe tags and escaped content
      "<div>Safe &amp; sound</div>",
      // 'script' as substring but not a tag
      "this is a scriptless string with word script inside",
      // protocol-like string that is safe as plain text (not an attribute)
      "JaVaScRiPt:alert(1)",
      // whitespace and punctuation heavy content
      "   \n  \t  ",
      """<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII" style="width: 100%; max-width: 1071px; height: auto; max-height: 590px;"></p>""".stripMargin
    )

    safeSamples.zipWithIndex.foreach { case (v, idx) =>
      assert(HtmlSanitizer.isXssSafe(clue(v)), s"expected safeSamples($idx) to be safe: $v")
    }
  }

  test("HtmlSanitizer.isXssUnsafe") {
    // A more exhaustive list of known XSS payload variants that should be detected
    val unsafeSamples = List(
      "<script>alert(1)</script>",
      "<script>alert(1)//.",
      "<b>test</b><script>alert(1)</script>",
      // mixed-case and obfuscated javascript pseudo-protocols
      """<IMG SRC=\"javascript:alert('XSS');\"/>""",
      "<script SRC=http://xss.rocks/xss.js></script>",
      // JSON embedding containing script content
      """{"html":"<script>alert(1)</script>","safe":true}""",
      // tabs/newlines and obfuscated 'javascript'
      """<img src=\"jav\tascript:alert(1)\">""",
      """<img src=x onerror=alert(1)//>""",
      // broken line-separated javascript payloads
      """<img src=\"jav\nascript:alert(1)\">""",
      // event handlers
      "<a href=\"#\" onclick=\"alert(1)\">x</a>",
      "<div onmouseover=\"alert(1)\">hover</div>",
      "<body onload=alert(1)>",
      // svg/onload vector
      "<svg onload=alert(1)></svg>",
      // iframe/javascript src
      "<iframe src=\"javascript:alert(1)\"></iframe>",
      // data URI with HTML containing script
      "data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==",
      // style with javascript in url()
      "<div style=\"background-image: url(javascript:alert(1))\"></div>",
      // img with src using expression-like attempts
      "<img src=\"#\" onerror=\"this.src='javascript:alert(1)'\" />",
      // script in attribute values
      "<div data='</script><script>alert(1)</script>'></div>",
      // inline JS in href
      "<a href=\"javascript:alert('XSS')\">x</a>",
      // nested harmless tag with an embedded script string
      "<p>safe</p><script>evil()</script>",
      // malformed but dangerous-looking tags
      "<scr<script>ipt>alert(1)</scr</script>ipt>",
      // html comment wrapping a script tag (should still be unsafe)
      "<!--<script>alert(1)</script>-->",
      // onerror with various quoting styles
      "<img src=\"x\" onerror=alert('XSS')>",
      // minimal obfuscated vectors
      "<svg><script>alert(1)</script></svg>"
    )

    unsafeSamples.zipWithIndex.foreach { case (v, idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(v)), s"expected unsafeSamples($idx) to be unsafe: $v")
    }
  }
}
