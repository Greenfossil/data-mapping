package com.greenfossil.data.mapping

import munit.FunSuite

class HtmlSanitizerSuite extends FunSuite:

  test("HtmlSanitizer.sanitizeWithFeedback - null input") {
    val result = HtmlSanitizer.sanitizeWithFeedback(null)
    assertEquals(result, null)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - safe input (no XSS)") {
    val input = "<p>Hello World</p>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - safe mixed HTML and text") {
    val input = "<p>Dear Homer,</p><p>Please proceed with your order.</p>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - script tag detected") {
    val input = "<script>alert(1)</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)

    // Should contain escaped original and wrapper
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;script&gt;alert(1)&lt;/script&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - script with src") {
    val input = "<script SRC=http://xss.rocks/xss.js></script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;script SRC&#61;http://xss.rocks/xss.js&gt;&lt;/script&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onerror event handler") {
    val input = "<img src=x onerror=alert(1)>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // img tag is allowed but onerror is removed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;x onerror&#61;alert(1)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onload event handler") {
    val input = "<svg onload='alert(1)'>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // svg is not allowed, should be removed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;svg onload&#61;&#39;alert(1)&#39;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - javascript: in src attribute") {
    val input = """<IMG SRC="javascript:alert('XSS');"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC&#61;&#34;javascript:alert(&#39;XSS&#39;);]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - javascript: mixed case") {
    val input = """<IMG SRC=JaVaScRiPt:alert('XSS')>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC&#61;JaVaScRiPt:alert(&#39;XSS&#39;)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - percent-encoded javascript") {
    val input = """<IMG SRC="javascript%3Aalert('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC&#61;&#34;javascript%3Aalert(&#39;XSS&#39;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - hex entity encoded javascript") {
    val input = """<IMG SRC="jav&#x61;script:alert('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC&#61;&#34;jav&amp;#x61;script:alert(&#39;XSS&#39;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - decimal entity encoded javascript") {
    val input = """<IMG SRC=jav&#9;ascript:alert('XSS')>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC&#61;jav&amp;#9;ascript:alert(&#39;XSS&#39;)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - comment obfuscation") {
    val input = """<IMG SRC="jav<!-- -->ascript:alert('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC&#61;&#34;jav&lt;!-- --&gt;ascript:alert(&#39;XSS&#39;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - data URI with base64 script") {
    val input = """<a href="data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">link</a>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // data: protocol not allowed, href attribute is removed. Since <a> is still allowed but href is removed, it will show as <a>link</a>
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;a href&#61;&#34;data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg&#61;&#61;&#34;&gt;link&lt;/a&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onmouseover event") {
    val input = """<IMG onmouseover="alert('xxs')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // img is allowed but onmouseover is removed, and src is required
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG onmouseover&#61;&#34;alert(&#39;xxs&#39;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - multiple XSS vectors") {
    val input = """<b>test</b><script>alert(1)</script><img onerror=alert(2)>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // script is removed with wrapper, img onerror is removed, b is allowed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;b&gt;test&lt;/b&gt;&lt;script&gt;alert(1)&lt;/script&gt;&lt;img onerror&#61;alert(2)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - allowed link with safe href") {
    val input = """<a href="http://example.com">click</a>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - img with safe src") {
    val input = """<img src="http://example.com/image.jpg" alt="test" />"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - disallowed element") {
    val input = """<div>safe</div><style>body { color: red; }</style>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // div is allowed, style is not allowed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;div&gt;safe&lt;/div&gt;&lt;style&gt;body { color: red; }&lt;/style&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - custom wrapper tag") {
    val input = "<script>alert(1)</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input, wrapperTag = "div")
    assertNoDiff(result, """<div class="xss-detected">[XSS Detected: &lt;script&gt;alert(1)&lt;/script&gt;]</div>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - custom wrapper class") {
    val input = "<script>alert(1)</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input, wrapperClass = "malicious-content")
    assertNoDiff(result, """<span class="malicious-content">[XSS Detected: &lt;script&gt;alert(1)&lt;/script&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - empty string") {
    val result = HtmlSanitizer.sanitizeWithFeedback("")
    assertNoDiff(result, "")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - whitespace only") {
    val result = HtmlSanitizer.sanitizeWithFeedback("   ")
    assertNoDiff(result, "   ")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - JSON object (false positive prevention)") {
    val json = """{"name":"John","age":30}"""
    val result = HtmlSanitizer.sanitizeWithFeedback(json)
    assertNoDiff(result, json)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - JSON array (false positive prevention)") {
    val json = """[1,2,3,4,5]"""
    val result = HtmlSanitizer.sanitizeWithFeedback(json)
    assertNoDiff(result, json)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - JSON with HTML-like content") {
    val json = """{"html":"<script>alert(1)</script>","safe":true}"""
    val result = HtmlSanitizer.sanitizeWithFeedback(json)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: {&#34;html&#34;:&#34;&lt;script&gt;alert(1)&lt;/script&gt;&#34;,&#34;safe&#34;:true}]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - plain text with no HTML markers") {
    val text = "This is just plain text with no HTML"
    val result = HtmlSanitizer.sanitizeWithFeedback(text)
    assertNoDiff(result, text)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - plain text with HTML-like string but no angle brackets") {
    val text = "Someone wrote javascript:alert(1) in email"
    val result = HtmlSanitizer.sanitizeWithFeedback(text)
    assertNoDiff(result, text)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - iframe disallowed") {
    val input = """<iframe src="http://evil.com"></iframe>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;iframe src&#61;&#34;http://evil.com&#34;&gt;&lt;/iframe&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - form disallowed") {
    val input = """<form action="http://evil.com"><input type="text"></form>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;form action&#61;&#34;http://evil.com&#34;&gt;&lt;input type&#61;&#34;text&#34;&gt;&lt;/form&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onclick event on allowed element") {
    val input = """<p onclick="alert(1)">Click me</p>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // p is allowed but onclick is removed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;p onclick&#61;&#34;alert(1)&#34;&gt;Click me&lt;/p&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - inline style is allowed") {
    val input = """<p style="color:red">Text</p>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // p is allowed but style attribute is removed
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - escaped HTML entities as input") {
    val input = "&lt;script&gt;alert(1)&lt;/script&gt;"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // Already escaped - has & but no < so won't trigger XSS detection
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - mixed safe and unsafe elements") {
    val input = """<p>Safe content</p><script>alert(1)</script><b>More safe</b>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // script is removed, p and b are allowed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;p&gt;Safe content&lt;/p&gt;&lt;script&gt;alert(1)&lt;/script&gt;&lt;b&gt;More safe&lt;/b&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - deeply nested allowed elements") {
    val input = """<div><p><b><i>Nested text</i></b></p></div>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - escaped special characters in payload") {
    val input = """<img src="x" onerror="alert(&#34;XSS&#34;)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;&#34;x&#34; onerror&#61;&#34;alert(&amp;#34;XSS&amp;#34;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - multiple event handlers") {
    val input = """<img src="x" onload="alert(1)" onerror="alert(2)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;&#34;x&#34; onload&#61;&#34;alert(1)&#34; onerror&#61;&#34;alert(2)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - tab character in obfuscation") {
    val input = """<img src="jav	ascript:alert(1)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;&#34;jav	ascript:alert(1)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - newline in obfuscation") {
    val input =
      """<img src="jav
ascript:alert(1)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;&#34;jav
ascript:alert(1)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - vbscript protocol") {
    val input = """<img src="vbscript:msgbox('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;&#34;vbscript:msgbox(&#39;XSS&#39;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - malformed HTML with XSS") {
    val input = """<img src=x onerror=alert(1)//>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;x onerror&#61;alert(1)//&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - SVG with onload") {
    val input = """<svg><circle onload="alert(1)"/></svg>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;svg&gt;&lt;circle onload&#61;&#34;alert(1)&#34;/&gt;&lt;/svg&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - MathML with xlink") {
    val input = """<math><mi xlink:href="javascript:alert(1)"/></math>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;math&gt;&lt;mi xlink:href&#61;&#34;javascript:alert(1)&#34;/&gt;&lt;/math&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - escaped wrapper tag and class in payload") {
    val input = """<img src="x" onerror="alert('<span class=xss-detected>')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src&#61;&#34;x&#34; onerror&#61;&#34;alert(&#39;&lt;span class&#61;xss-detected&gt;&#39;)&#34;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - large payload") {
    val largePayload = "<script>" + "alert(1);" * 1000 + "</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(largePayload)
    assert(result.contains("XSS Detected"))
    assert(result.contains("&lt;script&gt;alert"))
  }

  test("HtmlSanitizer.sanitizeWithFeedback - with comment out") {
    val largePayload = "<script>" + "alert(1);//"
    val result = HtmlSanitizer.sanitizeWithFeedback(largePayload)
    assert(result.contains("XSS Detected"))
    assert(result.contains("&lt;script&gt;alert"))
  }


