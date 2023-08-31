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


class MappingBind1_FieldMapping_HtmlSuite extends munit.FunSuite {

  import Mapping.*

  test("html") {
    val field = htmlText.name("field")

    val boundMissingField = field.bind()
    assertEquals(boundMissingField.errors.head.message, "error.required")
    assertEquals(boundMissingField.typedValueOpt, None)

    val boundEmptyField = field.bind("field" -> "")
    assertEquals(boundEmptyField.errors, Nil)
    assertEquals(boundEmptyField.typedValueOpt, Option(""))
  }

  test("html - script-tag") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundScriptTagField1 = field.bind("field" -> "<script></script>")
    assertEquals(boundScriptTagField1.errors, Nil)
    assertEquals(boundScriptTagField1.typedValueOpt, Option(""))

    val boundScriptTagField2 = field.bind("field" -> "<script SRC=http://xss.rocks/xss.js></script>")
    assertEquals(boundScriptTagField2.errors, Nil)
    assertEquals(boundScriptTagField2.typedValueOpt, Option(""))

    val boundScriptTagField3 = field.bind("field" -> "<b>test</b><script>alert(1)</script>")
    assertEquals(boundScriptTagField3.errors, Nil)
    assertEquals(boundScriptTagField3.typedValueOpt, Option("<b>test</b>"))
  }

  test("html - on-event attribute") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundOnEventAttrField1 = field.bind("field" -> "<svg onload='alert(1)'>")
    assertEquals(boundOnEventAttrField1.errors, Nil)
    assertEquals(boundOnEventAttrField1.typedValueOpt, Option("<svg >"))

    val boundOnEventAttrField2 = field.bind("field" -> """<IMG onmouseover="alert('xxs')">""")
    assertEquals(boundOnEventAttrField2.errors, Nil)
    assertEquals(boundOnEventAttrField2.typedValueOpt, Option("<IMG >"))

    val boundOnEventAttrField3 = field.bind("field" -> """<a onmouseover="alert(document.cookie)"\>xxs link\</a\>""")
    assertEquals(boundOnEventAttrField3.errors, Nil)
    assertEquals(boundOnEventAttrField3.typedValueOpt, Option("""<a \>xxs link\</a\>"""))

    val boundOnEventAttrField4 = field.bind("field" -> """<BODY onload!#$%&()*~+-_.,:;?@[/|\]^`=alert("XSS")>""")
    assertEquals(boundOnEventAttrField4.errors, Nil)
    assertEquals(boundOnEventAttrField4.typedValueOpt, Option("<BODY >"))

    val boundOnEventAttrField5 = field.bind("field" -> """<svg/onload=alert('XSS')>""")
    assertEquals(boundOnEventAttrField5.errors, Nil)
    assertEquals(boundOnEventAttrField5.typedValueOpt, Option("<svg/>"))

    val boundOnEventAttrField6 = field.bind("field" -> """<h1 onload="">Testing</h1>""")
    assertEquals(boundOnEventAttrField6.errors, Nil)
    assertEquals(boundOnEventAttrField6.typedValueOpt, Option("<h1 >Testing</h1>"))
  }

  test("html - src attribute") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundSrcAttrField1 = field.bind("field" -> """<IMG SRC="javascript:alert('XSS');">""")
    assertEquals(boundSrcAttrField1.errors, Nil)
    assertEquals(boundSrcAttrField1.typedValueOpt, Option("<IMG >"))

    val boundSrcAttrField2 = field.bind("field" -> """<IMG SRC=JaVaScRiPt:alert('XSS')>""")
    assertEquals(boundSrcAttrField2.errors, Nil)
    assertEquals(boundSrcAttrField2.typedValueOpt, Option("<IMG >"))

    val boundSrcAttrField3 = field.bind("field" -> """<IMG SRC=javascript:alert('XSS')>""")
    assertEquals(boundSrcAttrField3.errors, Nil)
    assertEquals(boundSrcAttrField3.typedValueOpt, Option("<IMG >"))

    val boundSrcAttrField4 = field.bind("field" -> """<IMG SRC="javascript:alert('XSS');">""")
    assertEquals(boundSrcAttrField4.errors, Nil)
    assertEquals(boundSrcAttrField4.typedValueOpt, Option("<IMG >"))

    val boundSrcAttrField5 = field.bind("field" -> """<IMG SRC=`javascript:alert("RSnake says, 'XSS'")`>""")
    assertEquals(boundSrcAttrField5.errors, Nil)
    assertEquals(boundSrcAttrField5.typedValueOpt, Option("<IMG >"))

    val boundSrcAttrField6 = field.bind("field" -> """<IMG \"\"\"><SCRIPT>alert("XSS")</SCRIPT>"\>""")
    assertEquals(boundSrcAttrField6.errors, Nil)
    assertEquals(boundSrcAttrField6.typedValueOpt, Option("<IMG \\\"\\\"\\\">\"\\>"))

    val boundSrcAttrField7 = field.bind("field" -> """<IMG SRC= onmouseover="alert('xxs')">""")
    assertEquals(boundSrcAttrField7.errors, Nil)
    assertEquals(boundSrcAttrField7.typedValueOpt, Option("<IMG >"))

    val boundSrcAttrField8 = field.bind("field" -> """<IMG SRC=# onmouseover="alert('xxs')">""")
    assertEquals(boundSrcAttrField8.errors, Nil)
    assertEquals(boundSrcAttrField8.typedValueOpt, Option("<IMG >"))
  }

  test("html - Dom purify samples") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundOnEventAttrField1 = field.bind("field" -> "<img src=x onerror=alert(1)//>")
    assertEquals(boundOnEventAttrField1.errors, Nil)
    assertEquals(boundOnEventAttrField1.typedValueOpt, Option("<img //>"))

    val boundOnEventAttrField2 = field.bind("field" -> "<svg><g/onload=alert(2)//<p>")
    assertEquals(boundOnEventAttrField2.errors, Nil)
    assertEquals(boundOnEventAttrField2.typedValueOpt, Option("<svg><g///<p>"))

    val boundOnEventAttrField3 = field.bind("field" -> "<p>abc<iframe//src=jAva&Tab;script:alert(3)>def</p>")
    assertEquals(boundOnEventAttrField3.errors, Nil)
    assertEquals(boundOnEventAttrField3.typedValueOpt, Option("<p>abc<iframe//>def</p>"))

    val boundOnEventAttrField4 = field.bind("field" -> """<math><mi//xlink:href="data:x,<script>alert(4)</script>">""")
    assertEquals(boundOnEventAttrField4.errors, Nil)
    assertEquals(boundOnEventAttrField4.typedValueOpt, Option("<math><mi//xlink:href=\"data:x,\">"))

    val boundOnEventAttrField5 = field.bind("field" -> "<TABLE><tr><td>HELLO</tr></TABL>")
    assertEquals(boundOnEventAttrField5.errors, Nil)
    assertEquals(boundOnEventAttrField5.typedValueOpt, Option("<TABLE><tr><td>HELLO</tr></TABL>"))

    val boundOnEventAttrField6 = field.bind("field" -> "<UL><li><A HREF=//google.com>click</UL>")
    assertEquals(boundOnEventAttrField6.errors, Nil)
    assertEquals(boundOnEventAttrField6.typedValueOpt, Option("<UL><li><A HREF=//google.com>click</UL>"))

  }

  test("html - Clean html and should not remove any text"){
    val htmlValue =
      """<p>Dear Homer,<p><p>Please proceed with the your order.
        |To confirm the delivery, please login to <a href="http://www.example.com/>your account</a>.""".stripMargin

    val form = htmlText.name("value").bind("value" -> htmlValue)
    assertEquals(form.typedValueOpt, Some(htmlValue))
  }

}
