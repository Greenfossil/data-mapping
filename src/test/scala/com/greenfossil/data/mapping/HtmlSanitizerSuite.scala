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
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;script&gt;alert(1)&lt;&#x2F;script&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - script with src") {
    val input = "<script SRC=http://xss.rocks/xss.js></script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;script SRC=http:&#x2F;&#x2F;xss.rocks&#x2F;xss.js&gt;&lt;&#x2F;script&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onerror event handler") {
    val input = "<img src=x onerror=alert(1)>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // img tag is allowed but onerror is removed
    assertNoDiff(result, """<img src="x" /><span class="xss-detected">[XSS Detected: &lt;img src=x onerror=alert(1)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onload event handler") {
    val input = "<svg onload='alert(1)'>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // svg is not allowed, should be removed
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;svg onload=&#x27;alert(1)&#x27;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - javascript: in src attribute") {
    val input = """<IMG SRC="javascript:alert('XSS');"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC=&quot;javascript:alert(&#x27;XSS&#x27;);]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - javascript: mixed case") {
    val input = """<IMG SRC=JaVaScRiPt:alert('XSS')>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC=JaVaScRiPt:alert(&#x27;XSS&#x27;)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - percent-encoded javascript") {
    val input = """<IMG SRC="javascript%3Aalert('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<img src="javascript%3Aalert%28&#39;XSS&#39;%29" /><span class="xss-detected">[XSS Detected: &lt;IMG SRC=&quot;javascript%3Aalert(&#x27;XSS&#x27;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - hex entity encoded javascript") {
    val input = """<IMG SRC="jav&#x61;script:alert('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC=&quot;jav&amp;#x61;script:alert(&#x27;XSS&#x27;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - decimal entity encoded javascript") {
    val input = """<IMG SRC=jav&#9;ascript:alert('XSS')>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC=jav&amp;#9;ascript:alert(&#x27;XSS&#x27;)&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - comment obfuscation") {
    val input = """<IMG SRC="jav<!-- -->ascript:alert('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG SRC=&quot;jav&lt;!-- --&gt;ascript:alert(&#x27;XSS&#x27;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - data URI with base64 script") {
    val input = """<a href="data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">link</a>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // data: protocol not allowed, href attribute is removed. Since <a> is still allowed but href is removed, it will show as <a>link</a>
    assertNoDiff(result, """link<span class="xss-detected">[XSS Detected: &lt;a href=&quot;data:text&#x2F;html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==&quot;&gt;link&lt;&#x2F;a&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onmouseover event") {
    val input = """<IMG onmouseover="alert('xxs')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // img is allowed but onmouseover is removed, and src is required
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;IMG onmouseover=&quot;alert(&#x27;xxs&#x27;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - multiple XSS vectors") {
    val input = """<b>test</b><script>alert(1)</script><img onerror=alert(2)>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // script is removed with wrapper, img onerror is removed, b is allowed
    assertNoDiff(result, """<b>test</b><span class="xss-detected">[XSS Detected: &lt;b&gt;test&lt;&#x2F;b&gt;&lt;script&gt;alert(1)&lt;&#x2F;script&gt;&lt;img onerror=alert(2)&gt;]</span>""")
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
    assertNoDiff(result, """<div>safe</div><span class="xss-detected">[XSS Detected: &lt;div&gt;safe&lt;&#x2F;div&gt;&lt;style&gt;body { color: red; }&lt;&#x2F;style&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - custom wrapper tag") {
    val input = "<script>alert(1)</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input, wrapperTag = "div")
    assertNoDiff(result, """<div class="xss-detected">[XSS Detected: &lt;script&gt;alert(1)&lt;&#x2F;script&gt;]</div>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - custom wrapper class") {
    val input = "<script>alert(1)</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(input, wrapperClass = "malicious-content")
    assertNoDiff(result, """<span class="malicious-content">[XSS Detected: &lt;script&gt;alert(1)&lt;&#x2F;script&gt;]</span>""")
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
    assertNoDiff(result, """{&#34;html&#34;:&#34;&#34;,&#34;safe&#34;:true}<span class="xss-detected">[XSS Detected: {&quot;html&quot;:&quot;&lt;script&gt;alert(1)&lt;&#x2F;script&gt;&quot;,&quot;safe&quot;:true}]</span>""")
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
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;iframe src=&quot;http:&#x2F;&#x2F;evil.com&quot;&gt;&lt;&#x2F;iframe&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - form disallowed") {
    val input = """<form action="http://evil.com"><input type="text"></form>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;form action=&quot;http:&#x2F;&#x2F;evil.com&quot;&gt;&lt;input type=&quot;text&quot;&gt;&lt;&#x2F;form&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - onclick event on allowed element") {
    val input = """<p onclick="alert(1)">Click me</p>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    // p is allowed but onclick is removed
    assertNoDiff(result, """<p>Click me</p><span class="xss-detected">[XSS Detected: &lt;p onclick=&quot;alert(1)&quot;&gt;Click me&lt;&#x2F;p&gt;]</span>""")
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
    assertNoDiff(result, """<p>Safe content</p><b>More safe</b><span class="xss-detected">[XSS Detected: &lt;p&gt;Safe content&lt;&#x2F;p&gt;&lt;script&gt;alert(1)&lt;&#x2F;script&gt;&lt;b&gt;More safe&lt;&#x2F;b&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - deeply nested allowed elements") {
    val input = """<div><p><b><i>Nested text</i></b></p></div>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, input)
  }

  test("HtmlSanitizer.sanitizeWithFeedback - escaped special characters in payload") {
    val input = """<img src="x" onerror="alert(&quot;XSS&quot;)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<img src="x" /><span class="xss-detected">[XSS Detected: &lt;img src=&quot;x&quot; onerror=&quot;alert(&amp;quot;XSS&amp;quot;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - multiple event handlers") {
    val input = """<img src="x" onload="alert(1)" onerror="alert(2)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<img src="x" /><span class="xss-detected">[XSS Detected: &lt;img src=&quot;x&quot; onload=&quot;alert(1)&quot; onerror=&quot;alert(2)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - tab character in obfuscation") {
    val input = """<img src="jav	ascript:alert(1)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src=&quot;jav	ascript:alert(1)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - newline in obfuscation") {
    val input =
      """<img src="jav
ascript:alert(1)">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src=&quot;jav
ascript:alert(1)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - vbscript protocol") {
    val input = """<img src="vbscript:msgbox('XSS')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;img src=&quot;vbscript:msgbox(&#x27;XSS&#x27;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - malformed HTML with XSS") {
    val input = """<img src=x onerror=alert(1)//>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<img src="x" /><span class="xss-detected">[XSS Detected: &lt;img src=x onerror=alert(1)&#x2F;&#x2F;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - SVG with onload") {
    val input = """<svg><circle onload="alert(1)"/></svg>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;svg&gt;&lt;circle onload=&quot;alert(1)&quot;&#x2F;&gt;&lt;&#x2F;svg&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - MathML with xlink") {
    val input = """<math><mi xlink:href="javascript:alert(1)"/></math>"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<span class="xss-detected">[XSS Detected: &lt;math&gt;&lt;mi xlink:href=&quot;javascript:alert(1)&quot;&#x2F;&gt;&lt;&#x2F;math&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - escaped wrapper tag and class in payload") {
    val input = """<img src="x" onerror="alert('<span class=xss-detected>')">"""
    val result = HtmlSanitizer.sanitizeWithFeedback(input)
    assertNoDiff(result, """<img src="x" /><span class="xss-detected">[XSS Detected: &lt;img src=&quot;x&quot; onerror=&quot;alert(&#x27;&lt;span class=xss-detected&gt;&#x27;)&quot;&gt;]</span>""")
  }

  test("HtmlSanitizer.sanitizeWithFeedback - large payload") {
    val largePayload = "<script>" + "alert(1);" * 1000 + "</script>"
    val result = HtmlSanitizer.sanitizeWithFeedback(largePayload)
    assert(result.contains("XSS Detected"))
    assert(result.contains("&lt;script&gt;alert"))
  }

