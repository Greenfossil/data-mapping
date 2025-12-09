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
    assertEquals(boundScriptTagField1.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundScriptTagField1.typedValueOpt, Option("<script></script>"))

    val boundScriptTagField2 = field.bind("field" -> "<script SRC=http://xss.rocks/xss.js></script>")
    assertEquals(boundScriptTagField2.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundScriptTagField2.typedValueOpt, Option("<script SRC=http://xss.rocks/xss.js></script>"))

    val boundScriptTagField3 = field.bind("field" -> "<b>test</b><script>alert(1)</script>")
    assertEquals(boundScriptTagField3.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundScriptTagField3.typedValueOpt, Option("<b>test</b><script>alert(1)</script>"))
  }

  test("html - on-event attribute") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundOnEventAttrField1 = field.bind("field" -> "<svg onload='alert(1)'>")
    assertEquals(boundOnEventAttrField1.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField1.typedValueOpt, Option("<svg onload='alert(1)'>"))

    val boundOnEventAttrField2 = field.bind("field" -> """<IMG onmouseover="alert('xxs')">""")
    assertEquals(boundOnEventAttrField2.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField2.typedValueOpt, Option("""<IMG onmouseover="alert('xxs')">"""))

    val boundOnEventAttrField3 = field.bind("field" -> """<a onmouseover="alert(document.cookie)"\>xxs link\</a\>""")
    assertEquals(boundOnEventAttrField3.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField3.typedValueOpt, Option("""<a onmouseover="alert(document.cookie)"\>xxs link\</a\>"""))

    val boundOnEventAttrField4 = field.bind("field" -> """<BODY onload!#$%&()*~+-_.,:;?@[/|\]^`=alert("XSS")>""")
    assertEquals(boundOnEventAttrField4.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField4.typedValueOpt, Option("""<BODY onload!#$%&()*~+-_.,:;?@[/|\]^`=alert("XSS")>"""))

    val boundOnEventAttrField5 = field.bind("field" -> """<svg/onload=alert('XSS')>""")
    assertEquals(boundOnEventAttrField5.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField5.typedValueOpt, Option("""<svg/onload=alert('XSS')>"""))

    val boundOnEventAttrField6 = field.bind("field" -> """<h1 onload="">Testing</h1>""")
    assertEquals(boundOnEventAttrField6.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField6.typedValueOpt, Option("""<h1 onload="">Testing</h1>"""))
  }

  test("html - src attribute") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundSrcAttrField1 = field.bind("field" -> """<IMG SRC="javascript:alert('XSS');">""")
    assertEquals(boundSrcAttrField1.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField1.typedValueOpt, Option("""<IMG SRC="javascript:alert('XSS');">"""))

    val boundSrcAttrField2 = field.bind("field" -> """<IMG SRC=JaVaScRiPt:alert('XSS')>""")
    assertEquals(boundSrcAttrField2.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField2.typedValueOpt, Option("""<IMG SRC=JaVaScRiPt:alert('XSS')>"""))

    val boundSrcAttrField3 = field.bind("field" -> """<IMG SRC=javascript:alert('XSS')>""")
    assertEquals(boundSrcAttrField3.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField3.typedValueOpt, Option("""<IMG SRC=javascript:alert('XSS')>"""))

    val boundSrcAttrField4 = field.bind("field" -> """<IMG SRC="javascript:alert('XSS');">""")
    assertEquals(boundSrcAttrField4.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField4.typedValueOpt, Option("""<IMG SRC="javascript:alert('XSS');">"""))

    val boundSrcAttrField5 = field.bind("field" -> """<IMG SRC=`javascript:alert("RSnake says, 'XSS'")`>""")
    assertEquals(boundSrcAttrField5.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField5.typedValueOpt, Option("""<IMG SRC=`javascript:alert("RSnake says, 'XSS'")`>"""))

    val boundSrcAttrField6 = field.bind("field" -> """<IMG \"\"\"><SCRIPT>alert("XSS")</SCRIPT>"\>""")
    assertEquals(boundSrcAttrField6.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField6.typedValueOpt, Option("""<IMG \"\"\"><SCRIPT>alert("XSS")</SCRIPT>"\>"""))

    val boundSrcAttrField7 = field.bind("field" -> """<IMG SRC= onmouseover="alert('xxs')">""")
    assertEquals(boundSrcAttrField7.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField7.typedValueOpt, Option("""<IMG SRC= onmouseover="alert('xxs')">"""))

    val boundSrcAttrField8 = field.bind("field" -> """<IMG SRC=# onmouseover="alert('xxs')">""")
    assertEquals(boundSrcAttrField8.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundSrcAttrField8.typedValueOpt, Option("""<IMG SRC=# onmouseover="alert('xxs')">"""))
  }

  test("html - Dom purify samples") {
    val field = htmlText.name("field")

    //Test Script Tag
    val boundOnEventAttrField1 = field.bind("field" -> "<img src=x onerror=alert(1)//>")
    assertEquals(boundOnEventAttrField1.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField1.typedValueOpt, Option("<img src=x onerror=alert(1)//>"))

    val boundOnEventAttrField2 = field.bind("field" -> "<svg><g/onload=alert(2)//<p>")
    assertEquals(boundOnEventAttrField2.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField2.typedValueOpt, Option("<svg><g/onload=alert(2)//<p>"))

    val boundOnEventAttrField3 = field.bind("field" -> "<p>abc<iframe//src=jAva&Tab;script:alert(3)>def</p>")
    assertEquals(boundOnEventAttrField3.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField3.typedValueOpt, Option("<p>abc<iframe//src=jAva&Tab;script:alert(3)>def</p>"))

    val boundOnEventAttrField4 = field.bind("field" -> """<math><mi//xlink:href="data:x,<script>alert(4)</script>">""")
    assertEquals(boundOnEventAttrField4.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField4.typedValueOpt, Option("""<math><mi//xlink:href="data:x,<script>alert(4)</script>">"""))

    val boundOnEventAttrField5 = field.bind("field" -> "<TABLE><tr><td>HELLO</tr></TABL>")
    assertEquals(boundOnEventAttrField5.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField5.typedValueOpt, Option("<TABLE><tr><td>HELLO</tr></TABL>"))

    val boundOnEventAttrField6 = field.bind("field" -> "<UL><li><A HREF=//google.com>click</UL>")
    assertEquals(boundOnEventAttrField6.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundOnEventAttrField6.typedValueOpt, Option("<UL><li><A HREF=//google.com>click</UL>"))

  }

  test("html - encoding/obfuscation bypasses") {
    val field = htmlText.name("field")

    // Percent-encoded javascript: inside src
    val boundEnc1 = field.bind("field" -> """<IMG SRC=\"javascript%3Aalert('XSS')\">""")
    assertEquals(boundEnc1.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundEnc1.typedValueOpt, Option("""<IMG SRC=\"javascript%3Aalert('XSS')\">"""))

    // Hex/entity-encoded 'a' in 'javascript' (jav&#x61;script)
    val boundEnc2 = field.bind("field" -> """<IMG SRC=\"jav&#x61;script:alert('XSS')\">""")
    assertEquals(boundEnc2.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundEnc2.typedValueOpt, Option("""<IMG SRC=\"jav&#x61;script:alert('XSS')\">"""))

    // Decimal entity/tab between letters (jav&#9;ascript)
    val boundEnc3 = field.bind("field" -> """<IMG SRC=jav&#9;ascript:alert('XSS')>""")
    assertEquals(boundEnc3.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundEnc3.typedValueOpt, Option("""<IMG SRC=jav&#9;ascript:alert('XSS')>"""))

    // Comment-inserted obfuscation: jav<!-- -->ascript
    val boundEnc4 = field.bind("field" -> """<IMG SRC=\"jav<!-- -->ascript:alert('XSS')\">""")
    assertEquals(boundEnc4.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundEnc4.typedValueOpt, Option("""<IMG SRC=\"jav<!-- -->ascript:alert('XSS')\">"""))

    // data: URI with base64 encoded script tag
    val boundEnc5 = field.bind("field" -> """<a href=\"data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==\">link</a>""")
    assertEquals(boundEnc5.errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(boundEnc5.typedValueOpt, Option("""<a href=\"data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==\">link</a>"""))
  }

  test("html - Clean html and should not remove any text"){
    val htmlValue =
      """<p>Dear Homer,<p><p>Please proceed with the your order.
        |To confirm the delivery, please login to <a href="http://www.example.com/>your account</a>.""".stripMargin

    val form = htmlText.name("value").bind("value" -> htmlValue)
    assertEquals(form.typedValueOpt, Some(htmlValue))
  }

  test("Unclosed script tag") {
    val field = htmlText.name("value")

    val text1 = "Successfully updated <script>alert(1)//."
    assertEquals(field.bind("value" -> text1).errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(field.bind("value" -> text1).typedValueOpt, Option(text1))

    val text2 = "<script>a=document.createElement('a');a.href='data:text/plain,XSS';a.download='x.txt';a.click()//"
    assertEquals(field.bind("value" -> text2).errors.flatMap(_.messages), List("error.xss.detected"))
    assertEquals(field.bind("value" -> text2).typedValueOpt, Option(text2))
  }

}
