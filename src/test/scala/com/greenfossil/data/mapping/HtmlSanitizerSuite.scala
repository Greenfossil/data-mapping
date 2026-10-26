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

  /*
   * Regression coverage for the relative-href false positive:
   * the safelist must preserve relative / scheme-less links instead of stripping them,
   * otherwise isXssSafe reports any HTML containing such a link as unsafe.
   */
  test("HtmlSanitizer.isXssSafe - relative and scheme-less hrefs are safe") {
    val safeHrefs = List(
      // scheme-less host-looking href (typical of hand-written email templates)
      ("""<a href="www.example.com">x</a>""", "www.example.com"),
      // site-relative path
      ("""<a href="/we/index">x</a>""", "/we/index"),
      // parent-relative path
      ("""<a href="../admin">x</a>""", "../admin"),
      // in-page anchor
      ("""<a href="#anchor">x</a>""", "#anchor"),
      // fragment whose text looks like a scheme: a leading '#' can never be a scheme
      ("""<a href="#data:section">x</a>""", "#data:section"),
      // absolute https
      ("""<a href="https://example.com">x</a>""", "https://example.com"),
      // mailto
      ("""<a href="mailto:a@b.com">x</a>""", "mailto:a@b.com")
    )

    safeHrefs.zipWithIndex.foreach { case ((input, href), idx) =>
      assert(HtmlSanitizer.isXssSafe(clue(input)), s"expected safeHrefs($idx) to be safe: $input")
      // the href must survive sanitization unchanged (not stripped)
      assert(
        HtmlSanitizer.sanitize(input).contains(s"""href="$href""""),
        s"expected href [$href] to be preserved by sanitize, got: ${HtmlSanitizer.sanitize(input)}"
      )
    }
  }

  test("HtmlSanitizer.isXssSafe - unsafe href protocols are still detected") {
    val unsafeHrefs = List(
      """<a href="javascript:alert(1)">x</a>""",
      // mixed case must not bypass the protocol check
      """<a href="JaVaScRiPt:alert(1)">x</a>""",
      """<a href="data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">x</a>"""
    )

    unsafeHrefs.zipWithIndex.foreach { case (input, idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(input)), s"expected unsafeHrefs($idx) to be unsafe: $input")
      // the dangerous href must be removed by sanitization
      assert(
        !HtmlSanitizer.sanitize(input).toLowerCase.contains("href="),
        s"expected href to be stripped from unsafeHrefs($idx), got: ${HtmlSanitizer.sanitize(input)}"
      )
    }
  }

  /*
   * Browsers strip tab/CR/LF from URLs and treat '\' as '/', so these values are really
   * protocol-relative references to evil.com and must be treated as unsafe.
   */
  test("HtmlSanitizer.isXssSafe - hrefs that normalize to a protocol-relative URL are unsafe") {
    val unsafeHrefs = List(
      ("""<a href="//evil.com">x</a>""", "protocol-relative"),
      ("""<a href="\//evil.com">x</a>""", "backslash then protocol-relative"),
      ("""<a href="/\evil.com">x</a>""", "mixed leading slashes"),
      ("""<a href="\\evil.com">x</a>""", "backslashes only"),
      ("<a href=\"/\t/evil.com\">x</a>", "literal tab inside the target"),
      ("<a href=\"/\r/evil.com\">x</a>", "literal CR inside the target"),
      ("""<a href="&#92;&#92;evil.com">x</a>""", "entity-encoded backslashes"),
      // dangerous scheme hidden behind leading junk
      ("""<a href='"javascript:alert(1)'>x</a>""", "leading junk before javascript:")
    )

    unsafeHrefs.zipWithIndex.foreach { case ((input, label), idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(input)), s"expected unsafeHrefs($idx) [$label] to be unsafe: $input")
      assert(
        !HtmlSanitizer.sanitize(input).toLowerCase.contains("href="),
        s"expected href to be stripped from unsafeHrefs($idx) [$label], got: ${HtmlSanitizer.sanitize(input)}"
      )
    }
  }

  /*
   * A leading '/' or '#' makes the value a relative reference, so the href guard must not
   * read a scheme out of it. NOTE: isXssSafe() still reports these as unsafe because the
   * pre-existing containsMaliciousPatterns() heuristic scans the raw input and matches any
   * "file:"/"vbscript:"/"javascript:" token. That false positive predates the relative-link
   * work and is unrelated to the href guard, so this test asserts the guard's behaviour only.
   */
  test("HtmlSanitizer.sanitize - relative hrefs containing a scheme-like token are preserved") {
    val hrefs = List(
      """#file:x""",
      """#vbscript:x""",
      """/help/javascript:guide"""
    )

    hrefs.zipWithIndex.foreach { case (href, idx) =>
      val input = s"""<a href="$href">x</a>"""
      assert(HtmlSanitizer.containsMaliciousPatterns(input), s"expected hrefs($idx) to hit the raw-input heuristic: $input")
      assert(
        HtmlSanitizer.sanitize(input).contains(s"""href="$href""""),
        s"expected href [$href] to be preserved by sanitize, got: ${HtmlSanitizer.sanitize(input)}"
      )
    }
  }

  test("HtmlSanitizer.sanitize - template variable hrefs are preserved") {
    // The guard runs after the TEMPLATE_VAR_* restore, so a template placeholder must survive
    val input = """<a href="${customForm_123}">x</a>"""
    assert(HtmlSanitizer.isXssSafe(clue(input)), "expected a template variable href to be safe")
    assert(
      HtmlSanitizer.sanitize(input).contains("""href="${customForm_123}""""),
      s"expected the template variable href to be preserved, got: ${HtmlSanitizer.sanitize(input)}"
    )
  }

  test("HtmlSanitizer.isXssSafe - real email template with a scheme-less link is safe") {
    // Full payload reported by the field: the only attribute of interest is the scheme-less
    // <a href="www.example.com"> link, which used to be stripped, flipping isXssSafe to false.
    val template =
      """<h3>${displayName} </h3> <p>！，，，，</p> <p>，: Example.</p> <p><i>，，，, Example  Member Portal ！</i></p> <p></p> <table class="ui table" style="text-align: left; margin-left: auto; margin-right: auto;"> <tbody> <tr> <td style="border: 1px solid black; padding: 1em;"> <div style="text-align: center;"><b>Example:</b></div> <div style="text-align: center;"><b><br></b></div> <div style="text-align: center;">https://portal.example.com/</div> <div style="text-align: center;"><br></div> <div style="text-align: center;"><b><u>：</u></b></div> <div style="text-align: center;"><br></div> <div style="text-align: center;"><b>:</b><i>&lt;&lt;4 +  (YYYYMMDD)&gt;&gt;</i></div><div style="text-align: center;"><i><b> ()</b>: ${firstLoginUrl}<br></i></div> </td> </tr> </tbody> </table><br><p>，（Example）Member Portal Example ，Member Portal <a href="www.example.com">www.example.com</a>，， Example  ID  Example</p><p>，，</p><p>, hello@example.org  support@example.org</p><p>！</p><p>，<br>Example Team</p><p><br></p>"""

    assert(HtmlSanitizer.isXssSafe(clue(template)), "expected the real email template to be XSS-safe")
    assert(
      HtmlSanitizer.sanitize(template).contains("""href="www.example.com""""),
      s"expected the scheme-less href to be preserved, got: ${HtmlSanitizer.sanitize(template)}"
    )
  }

  /*
   * Regression: Welcome e-mail template body (reported as "Potential XSS attack detected").
   * It is legitimate HTML that mixes template variables (`${displayName}`, `${firstLoginUrl}`),
   * `&nbsp;`, `mailto:` links, <u>, an emoji and a scheme-less
   * <a href="www.example.com"> link. Scope: realistic payload, template form field.
   */
  test("HtmlSanitizer.isXssSafe - welcome e-mail template body with template variables is safe") {
    val template = """<h3>${displayName} </h3> <p>！，，，，</p> <p>，: Example.</p> <p><i>，，， Example  Member Portal ！</i></p> <p></p> <table class="ui table" style="text-align: left; margin-left: auto; margin-right: auto;"> <tbody> <tr> <td style="border: 1px solid black; padding: 1em;"> <div style="text-align: center;"><b>Example:</b></div> <div style="text-align: center;"><b><br></b></div> <div style="text-align: center;">https://portal.example.com/</div> <div style="text-align: center;"><br></div> <div style="text-align: center;"><b><u>：</u></b></div> <div style="text-align: center;"><br></div> <div style="text-align: center;"><b>:</b><i>&lt;&lt;4 +  (YYYYMMDD)&gt;&gt;</i></div><div style="text-align: center;"><i><b>Password (please click on URL)</b>: ${firstLoginUrl}<br></i></div> </td> </tr> </tbody> </table> <p></p> <p>，（Example）Member Portal！ Example ，Member Portal&nbsp;<a href="www.example.com">www.example.com</a>，， Example  ID  Example</p> <p></p><p></p> <p>，，</p><p>, <a href="mailto:hello@example.org" style="text-decoration-line: underline;">hello@example.org</a>&nbsp;&nbsp;<u><a href="mailto:support@example.org">support@example.org</a></u></p> <p></p> <p><i>！<br><br>，<br>Example Team</i><br></p>"""

    assert(HtmlSanitizer.isXssSafe(clue(template)), "expected the welcome e-mail template to be XSS-safe")
    assert(!HtmlSanitizer.containsUnsafe(template), "expected containsUnsafe to be false for a legitimate HTML e-mail template")

    // The template form (TemplateController.templateForm) binds the body with nonEmptyText(1, 15000, true)
    val bound = Mapping.nonEmptyText(1, 15000, true).name("body").bind("body" -> template)
    assertEquals(bound.errors.map(_.message), Nil)
    assert(bound.typedValueOpt.nonEmpty)

    val cleaned = HtmlSanitizer.sanitize(template)
    // scheme-less href, mailto: links and template variables must all survive sanitization
    assert(cleaned.contains("""href="www.example.com""""), s"expected the scheme-less href to be preserved, got: $cleaned")
    assert(cleaned.contains("""href="mailto:hello@example.org""""), s"expected the mailto href to be preserved, got: $cleaned")
    assert(cleaned.contains("""href="mailto:support@example.org""""), s"expected the second mailto href to be preserved, got: $cleaned")
    assert(cleaned.contains("${displayName}"), s"expected the $${displayName} placeholder to be preserved, got: $cleaned")
  }

  /*
   * Regression: plain-text e-mail quote (Outlook style) saved into a remarks/text field
   * (reported from an unbounded remarks field using optional(text)). The `<user@host>` tokens
   * are e-mail addresses, not markup, but Jsoup parsed them as tags and dropped them, so
   * sanitization "changed" the input and the field was rejected with "Potential XSS attack detected".
   * Scope: realistic payload, plus the real text-mapping binding path.
   */
  test("HtmlSanitizer.isXssSafe - plain-text e-mail quote with angle-bracket addresses is safe") {
    val remarks = """14 Nov 2025- Updated status as 'inactive' as CL, Alice Tan has updated on 14 Nov 2025 hat she has left church together with her husband, Bob Lim. 

From: Alice Tan <alice.tan@example.com>
Date: Thu, 13 Nov 2025 at 23:01
Subject: Re: Example Org: Feedback from Tan, Alice
To: Bob Lim <bob.lim@example.org>
Cc: team@example.org <team@example.org>, chris.88@example.com <chris.88@example.com>, yan.mh@example.com <yan.mh@example.com>


Hi Bob,

Thanks for getting back. Have added my other 2 Core Team members to this thread.

Will fill out best to our knowledge.

Status:-
Carol Ng: Left Cell
David Ong: Left Cell
Erin Chua: Left Cell
Bob Lim: Changed church
Frank Goh: Changed church
Grace Tan: Changed church
Henry Ng:  Changed church
Ivy Chan: Changed church

Hope this helps.

Alex"""

    // pure prose: no markup at all, so it must never be treated as HTML
    assert(HtmlSanitizer.isXssSafe(clue(remarks)), "expected a plain-text e-mail quote to be XSS-safe")
    assert(!HtmlSanitizer.containsUnsafe(remarks), "expected containsUnsafe to be false for a plain-text e-mail quote")

    // a text mapping carries the XSS constraint, so binding it must succeed with no error
    val textBound = Mapping.text.name("remarks").bind("remarks" -> remarks)
    assertEquals(textBound.errors.map(_.message), Nil)
    assertEquals(textBound.typedValueOpt, Some(remarks.trim))

    // optional(text) is used by the same style of remarks field and must behave identically
    val optionalBound = Mapping.optional(Mapping.text).name("remarks").bind("remarks" -> remarks)
    assertEquals(optionalBound.errors.map(_.message), Nil)
    assertEquals(optionalBound.typedValueOpt.flatten, Some(remarks.trim))
  }

  /*
   * Negative controls for the unknown-element unwrapping: treating a bogus, address-like tag as
   * text must not swallow, hide or weaken real markup. Scope: classic XSS vectors plus payloads
   * that mix an e-mail address with executable markup.
   */
  test("HtmlSanitizer.isXssSafe - XSS is still detected alongside address-like text") {
    val unsafeInputs = List(
      "<script>alert(1)</script>" -> "plain script tag",
      "<img src=x onerror=alert(1)>" -> "event handler attribute",
      """<a href="javascript:alert(1)">x</a>""" -> "javascript: href",
      "From: Alice Tan <alice.tan@example.com>, plus <script>alert(1)</script>" -> "e-mail address followed by a script tag",
      "From: a@b.com <b@c.co onerror=alert(1)>" -> "event handler hidden behind an address-like token"
    )

    unsafeInputs.zipWithIndex.foreach { case ((input, label), idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(input)), s"expected unsafeInputs($idx) [$label] to be unsafe: $input")
      assert(HtmlSanitizer.containsUnsafe(input), s"expected containsUnsafe to be true for unsafeInputs($idx) [$label]")
    }
  }

  /*
   * Positive control: a single e-mail address inside a text field must not stop the rest of the
   * field from being treated as HTML. Scope: proves the unwrap-based normalisation handles mixed
   * content (which a "no known tags, therefore plain text" shortcut would not).
   */
  test("HtmlSanitizer.isXssSafe - e-mail address mixed with allowed HTML is safe") {
    val input = "Hello <b>world</b>, contact <help@example.org> for details"
    assert(HtmlSanitizer.isXssSafe(clue(input)), "expected allowed HTML mixed with an e-mail address to be safe")
    assert(HtmlSanitizer.sanitize(input).contains("<b>world</b>"), "expected the allowed tag to survive sanitization")
  }

  /*
   * Documents the deliberate behaviour change: an element whose tag is not a real HTML element
   * (`Tag.isKnownTag(...) == false`) is inert plain text, so after unwrapping both sides compare
   * equal and it is accepted (sanitize still strips the bogus tag from its output).
   *
   * `<svg>` needs an explicit carve-out for exactly that reason: `Tag.isKnownTag("svg")` is also
   * false, but svg is a real, script-capable HTML element, so the `unsafeTags` check in
   * `isXssSafe` is what keeps it unsafe. `<math>` is inert foreign content and is accepted.
   *
   * Scope: pins the boundary between "inert unknown tag" and "dangerous real tag", plus nested
   * unknown elements (the linear unwrap must flatten each level in order).
   */
  test("HtmlSanitizer.isXssSafe - inert unknown elements are accepted while svg stays unsafe") {
    val inertInputs = List(
      "<my-widget>hello</my-widget>",
      "<math>x</math>",
      // nested unknown elements, and an allowed tag inside an unknown wrapper
      "<my-widget><b>x</b></my-widget>",
      "<x@y.com><x@y.com>t</x@y.com></x@y.com>",
      "<div><my-widget>a</my-widget></div>"
    )
    inertInputs.zipWithIndex.foreach { case (input, idx) =>
      assert(HtmlSanitizer.isXssSafe(clue(input)), s"expected inertInputs($idx) to be treated as plain text: $input")
      assert(!HtmlSanitizer.containsUnsafe(input), s"expected containsUnsafe to be false for inertInputs($idx): $input")
    }
    // sanitize still strips the bogus tag, keeping its text
    assertEquals(HtmlSanitizer.sanitize("<my-widget>hello</my-widget>"), "hello")

    val unsafeSvg = List(
      "<svg></svg>" -> "empty svg element",
      "<svg onload=alert(1)>" -> "svg with an event handler",
      "<svg/onload=alert(1)>" -> "self-closing svg with an event handler"
    )
    unsafeSvg.zipWithIndex.foreach { case ((input, label), idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(input)), s"expected unsafeSvg($idx) [$label] to be unsafe: $input")
    }
  }

  /*
   * Regression: whitespace that trails an angle-bracket address. `Jsoup.clean` (inside `sanitize`)
   * trims the trailing newline/space that remains after a bogus tag is dropped, so a strict DOM
   * comparison reported legitimate text as modified and fields raised "error.xss.detected".
   * Whitespace adjacent to a tag boundary is formatting, not content, and must not decide the XSS
   * verdict. Scope: the sanitizer plus the real binding paths used by remarks and HTML-mail fields
   * (`textAsIs`, `text`, `htmlText`), including content inside `<p>`/`<td>`.
   */
  test("HtmlSanitizer.isXssSafe - whitespace after an angle-bracket address is not a modification") {
    val cases: Seq[(String, Mapping[?])] = Seq(
      "From: A <a@b.co>\n" -> Mapping.textAsIs,
      "From: A <a@b.co> " -> Mapping.text,
      "<p>From: A <a@b.co>\n</p>" -> Mapping.htmlText,
      "<td>A <a@b.co>\n</td>" -> Mapping.htmlText
    )

    cases.zipWithIndex.foreach { case ((input, mapping), idx) =>
      val shown = input.replace("\n", "\\n")
      assert(HtmlSanitizer.isXssSafe(clue(input)), s"expected cases($idx) to be XSS-safe: $shown")
      assert(!HtmlSanitizer.containsUnsafe(input), s"expected containsUnsafe to be false for cases($idx): $shown")
      val bound = mapping.name("field").bind("field" -> input)
      assertEquals(bound.errors.map(_.message), Nil, s"expected cases($idx) to bind without errors: $shown")
    }
  }

  /*
   * Fail-closed branches of the plain-text heuristic: an unknown element is only treated as text
   * when it has no attributes and its parsed tag name contains no '<'. Anything else is markup and
   * must stay unsafe — including a benign attribute, so the rule cannot be relaxed accidentally.
   * Scope: pins each guard of `isPlainTextTokenElement`.
   */
  test("HtmlSanitizer.isXssSafe - attributed or malformed unknown elements stay unsafe") {
    val unsafeInputs = List(
      "<x@y.com onerror=alert(1)>" -> "address-like tag carrying an event handler attribute",
      """<my-widget class="c">x</my-widget>""" -> "unknown tag with a benign attribute",
      "<scr<script>ipt>alert(1)</scr</script>ipt>" -> "malformed tag name containing '<'"
    )

    unsafeInputs.zipWithIndex.foreach { case ((input, label), idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(input)), s"expected unsafeInputs($idx) [$label] to be unsafe: $input")
    }
  }

  /*
   * Fail-closed: an inert unknown wrapper must not hide real markup inside it. The wrapper is
   * unwrapped with its children kept, so a nested dangerous element still reaches the unsafe-tag /
   * event-attribute checks and the DOM comparison. Scope: guards the unwrap pass against becoming
   * a bypass.
   */
  test("HtmlSanitizer.isXssSafe - unknown wrapper does not hide its children") {
    val unsafeInputs = List(
      "<x@y.com><script>alert(1)</script>" -> "script nested in an address-like tag",
      "<x@y.com><img src=x onerror=alert(1)>" -> "image with an event handler nested in an address-like tag"
    )

    unsafeInputs.zipWithIndex.foreach { case ((input, label), idx) =>
      assert(HtmlSanitizer.isXssUnSafe(clue(input)), s"expected unsafeInputs($idx) [$label] to be unsafe: $input")
    }
  }

  /*
   * Performance guard for the linear unwrap. `Element.unwrap()` removes one child at a time and
   * re-indexes the sibling list on each call, so unwrapping N unknown siblings is quadratic:
   * measured ~0.5 s for 20 000 siblings and ~16.4 s for 100 000 (300 KB). The implementation
   * flattens each parent's child list once, which is linear and measures ~90 ms at 100 000
   * siblings. The 3 s bound therefore separates the two by a wide margin while staying generous
   * against slower CI machines. Scope: catches a regression back to per-element unwrapping, not
   * absolute performance.
   */
  test("HtmlSanitizer.isXssSafe - many unknown siblings are unwrapped linearly") {
    val input = "<x>" * 100000
    val start = System.nanoTime()
    val verdict = HtmlSanitizer.isXssSafe(input)
    val elapsedMs = (System.nanoTime() - start) / 1000000

    assert(Set(true, false).contains(verdict), "expected isXssSafe to return a plain boolean verdict")
    assert(elapsedMs < 3000, s"expected ${input.length} chars of unknown siblings to be handled in well under 3 s, took $elapsedMs ms")
  }

  /*
   * Robustness: pathological angle-bracket input must not blow up the sanitizer or the binding
   * path. These multi-KB runs of address-like text produce very large tag names, so this guards
   * against non-linear tokenizer/parser or unwrap behaviour (parser runaway, unbounded recursion)
   * rather than the regex recursion that the previous pattern-matching implementation suffered
   * from. Scope: (1) the sanitizer directly, (2) the real `Mapping.text` /
   * `Mapping.optional(Mapping.text)` binding path, and (3) a length-constrained mapping, which
   * must still report a normal validation error instead of blowing up.
   */
  test("HtmlSanitizer.isXssSafe - pathological address-like input is handled without runaway") {
    // (1) direct call: any Boolean verdict is acceptable, only a crash/overflow is a failure
    val unterminated = "<a@" + "b." * 4000
    val directVerdict =
      try HtmlSanitizer.isXssSafe(unterminated)
      catch
        case e: StackOverflowError =>
          fail(s"isXssSafe must not overflow the stack on ${unterminated.length} chars of unterminated address-like input", e)
    assert(Set(true, false).contains(directVerdict), "expected isXssSafe to return a plain boolean verdict")

    // same shape, but terminated with a '>' so the parser actually closes the tag
    val terminated = "<a@" + "bb." * 4000 + "zz>"

    // (2) real binding path: text and optional(text) must produce a typed value or an error, not a crash
    val textBound =
      try Mapping.text.bind("field" -> terminated)
      catch
        case e: StackOverflowError =>
          fail(s"Mapping.text.bind must not overflow the stack on ${terminated.length} chars", e)
    assert(
      textBound.typedValueOpt.isDefined || textBound.errors.nonEmpty,
      s"expected Mapping.text.bind to yield a typed value or a validation error, got errors=${textBound.errors.map(_.message)}"
    )

    val optionalBound =
      try Mapping.optional(Mapping.text).bind("field" -> terminated)
      catch
        case e: StackOverflowError =>
          fail(s"Mapping.optional(Mapping.text).bind must not overflow the stack on ${terminated.length} chars", e)
    assert(
      optionalBound.typedValueOpt.isDefined || optionalBound.errors.nonEmpty,
      s"expected Mapping.optional(text).bind to yield a typed value or a validation error, got errors=${optionalBound.errors.map(_.message)}"
    )

    // (3) length-constrained mapping: the XSS check runs first, then maxLength reports normally
    val shortPayload = "<a@" + "b." * 200
    val lengthBound =
      try Mapping.text(1, 50, true).bind("field" -> shortPayload)
      catch
        case e: StackOverflowError =>
          fail(s"Mapping.text(1, 50, true).bind must not overflow the stack on ${shortPayload.length} chars", e)
    assert(
      lengthBound.errors.nonEmpty,
      s"expected a maxLength validation error for a ${shortPayload.length}-char value, got ${lengthBound.errors.map(_.message)}"
    )
  }
