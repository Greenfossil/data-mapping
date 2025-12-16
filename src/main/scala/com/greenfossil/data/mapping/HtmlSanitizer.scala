package com.greenfossil.data.mapping

import com.fasterxml.jackson.databind.ObjectMapper
import org.owasp.html.{Encoding, HtmlPolicyBuilder, PolicyFactory}
import java.util.Base64
import java.net.URLDecoder

/**
 * Centralized OWASP HTML sanitizer utility.
 * - `sanitize` returns cleaned HTML (or null for null input).
 * - `containsUnsafe` returns true when the sanitizer removed or altered input (indicating unsafe content).
 */
object HtmlSanitizer:
  private val policy: PolicyFactory =
    new HtmlPolicyBuilder()
      .allowElements(
        "a", "b", "i", "u", "strong", "em", "p", "ul", "ol", "li", "br",
        "div", "span", "img", "pre", "code", "blockquote", "table", "thead",
        "tbody", "tr", "td", "th", "h1", "h2", "h3", "h4", "h5", "h6"
      )
      .allowAttributes("href").onElements("a")
      .allowAttributes("src", "alt", "title").onElements("img")
      .allowAttributes("style").globally()
      .allowUrlProtocols("http", "https", "mailto")
      // Do not allow any 'on*' event attributes or script-like protocols
      .toFactory()

  def sanitize(input: String): String =
    if input == null then null
    else policy.sanitize(input)

  /*
  * Remotely an HTML tag
  */
  val htmlLikeRegex =
    """(?i)<\s*/?\s*[a-z][a-z0-9]*\b(?:[^>]*>?|$)""".r

  // Simple helper: detect suspicious data: URIs. Conservative and lightweight.
  private def isDataUriUnsafe(input: String): Boolean =
    val s = Option(input).map(_.trim).getOrElse("")
    if !s.toLowerCase.startsWith("data:") then false
    else
      val comma = s.indexOf(',')
      if comma < 0 then true // malformed -> unsafe
      else
        val header = s.substring(5, comma).toLowerCase
        val payload = s.substring(comma + 1)

        // Allowlist common raster image types
        val safeImagePrefixes = Seq("image/png", "image/jpeg", "image/jpg", "image/gif", "image/webp")
        if safeImagePrefixes.exists(header.startsWith) then false

        // Immediately consider scriptable or HTML-like media types unsafe
        if header.contains("text/html") || header.contains("image/svg") || header.contains("javascript") then true

        val isBase64 = header.contains("base64")
        if isBase64 then
          try
            val payloadForDecode = if payload.length > 11000 then payload.take(11000) else payload
            val decoded = new String(Base64.getDecoder.decode(payloadForDecode), "UTF-8").toLowerCase
            val markers = Seq("<script", "javascript:", "onload=", "<svg", "<!doctype")
            markers.exists(decoded.contains)
          catch
            case _: IllegalArgumentException => true // bad base64 -> unsafe
        else
          try
            val decoded = URLDecoder.decode(payload, "UTF-8").toLowerCase
            val markers = Seq("<script", "<svg", "javascript:", "onload=")
            markers.exists(decoded.contains)
          catch
            case _: Exception => true

  inline def isXssUnSafe(input: String): Boolean =
    !isXssSafe(input)

  def isXssSafe(input: String): Boolean =
    // Normalize/guard null and short-circuit data: URIs first
    val t = if input == null then "" else input.trim
    if t.toLowerCase.startsWith("data:") then
      !isDataUriUnsafe(t)
    else if t.isBlank || htmlLikeRegex.findFirstIn(t).isEmpty then true
    else
      /*
       * Sanitize and compare with original input
       * If sanitization did not modify input, it is considered safe
       * If sanitization modified input, decode sanitized and compare with input
       */
      val nonNbspInput = t.replaceAll("&nbsp;", " ") /*This is to handle Trumbowyg's auto encoding space character to nbsp*/
      val sanitized = sanitize(nonNbspInput)
      if nonNbspInput.equals(sanitized) then true
      else
        val decoded = Encoding.decodeHtml(sanitized, false)
        /*
         * Input is considered safe if sanitization did not modify it.
         * Use the decoded sanitized to compare with nonNbspInput.
         */
        nonNbspInput.equals(decoded)


  /** Sanitizes input and preserves the original unsafe fragment as escaped text for user feedback.
   * Returns sanitized HTML with the original offending payload shown escaped in a wrapper.
   * Safe to render: the original payload is HTML-escaped so it cannot execute.
   */
  def sanitizeWithFeedback(input: String,
                           wrapperTag: String = "span",
                           wrapperClass: String = "xss-detected"): String =
    if input == null then null
    else
      // Keep JSON false-positive avoidance
      if containsUnsafe(input) then {
        val encodedXss = encodeUnsafeXss(input)
        s"""<$wrapperTag class="$wrapperClass">[XSS Detected: $encodedXss]</$wrapperTag>"""
      }
      else input


  // Small inline escape helper
  @deprecated("Use encodeUnsafeXss instead")
  def escapeHtml(s: String): String =
    s.replace("&", "&amp;")
     .replace("<", "&lt;")
     .replace(">", "&gt;")
     .replace("\"", "&#34;")
     .replace("'", "&#39;")

  /**
   * Encodes untrusted input for safe display in HTML context.
   * @param untrustedInput
   * @return
   */
  def encodeUnsafeXss(untrustedInput: String): String =
    if untrustedInput == null then null
    else
      val sb = new java.lang.StringBuilder()
      Encoding.encodeRcdataOnto(untrustedInput,sb)
      sb.toString

  private val jsonMapper = new ObjectMapper()

  /** Returns true when sanitizer modified input (heuristic detection of unsafe content).
   * Avoid false positives for JSON by detecting and validating JSON before sanitizing.
   */
  def containsUnsafe(input: String): Boolean =
      // If there are no obvious HTML markers, treat as non-HTML.
      if isXssUnSafe(input) then true
      else {
        //Check if input is Json
        // If it looks like JSON, try to parse it; valid JSON is not HTML else return true since it is XssSafe
        val trimmed = input.trim
        if trimmed.startsWith("{") || trimmed.startsWith("[") then
          try
            jsonMapper.readTree(trimmed)
            false
          catch
            case _: Exception => false // not valid JSON but also no HTML markers -> treat as safe
        else
          false /*false means it is safe*/
      }

end HtmlSanitizer
