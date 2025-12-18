package com.greenfossil.data.mapping

import com.fasterxml.jackson.databind.ObjectMapper
import org.owasp.html.{AttributePolicy, Encoding, HtmlPolicyBuilder}
import org.slf4j.LoggerFactory

/**
 * Centralized OWASP HTML sanitizer utility.
 * - `sanitize` returns cleaned HTML (or null for null input).
 * - `containsUnsafe` returns true when the sanitizer removed or altered input (indicating unsafe content).
 */
object HtmlSanitizer:

  private val htmlSanitizerLogger = LoggerFactory.getLogger("data-mapping.HtmlSanitizer")

  val aHrefPolicy: AttributePolicy =
    (elementName: String, attributeName: String, value: String) => {
      if (value == null) null
      else {
        val v = value.trim
        // reject control chars/newlines
        if v.exists(ch => ch <= '\u001f' || ch == '\u007f') || v.contains('\n') || v.contains('\r') then null
        else
          try {
            val uri = new java.net.URI(v)
            Option(uri.getScheme).map(_.toLowerCase) match {
              case Some("http"|"https"|"mailto") => v
              case Some("data") =>
                // allow only image data URIs with base64 payload
                if (v.matches("^data:image/(png|jpe?g|gif|webp);base64,[A-Za-z0-9+/=]+$")) v else null
              case None =>
                // only allow site-relative paths
                if (v.startsWith("/")) v else null
              case _ => null // block javascript:, vbscript:, data:text/html, etc.
            }
          } catch {
            case _: Exception => null
          }
      }
    }

  val imgSrcPolicy: AttributePolicy =
    (elementName: String, attributeName: String, value: String) => {
      if value == null then null
      else {
        val v = value.trim
        // reject control chars/newlines
        if v.exists(ch => ch <= '\u001f' || ch == '\u007f') || v.contains('\n') || v.contains('\r') then null
        else
          try {
            val uri = new java.net.URI(v)
            Option(uri.getScheme).map(_.toLowerCase) match {
              case Some("http"|"https") =>
                // require host and a path that ends with an allowed image extension
                val hasHost = Option(uri.getHost).nonEmpty
                val path = Option(uri.getPath).getOrElse("")
                if hasHost && path.matches("(?i).+\\.(png|jpe?g|gif|webp)$") then v else null
              case Some("data") =>
                // allow only image data URIs with base64 payload
                if (v.matches("^data:image/(png|jpe?g|gif|webp);base64,[A-Za-z0-9+/=]+$")) v else null
              case _ => null
            }
          } catch {
            case _: Exception => null
          }
      }
    }

  val policy =
    new HtmlPolicyBuilder()
      .allowUrlProtocols("http", "https", "mailto", "data")
      .allowElements(
        "a", "b", "i", "u", "strong", "em", "p", "ul", "ol", "li", "br",
        "div", "span", "img", "pre", "code", "blockquote", "table", "thead",
        "tbody", "tr", "td", "th", "h1", "h2", "h3", "h4", "h5", "h6"
      )
      .allowAttributes("href").matching(aHrefPolicy).onElements("a")
      .allowAttributes("alt", "title", "width", "height").onElements("img")
      .allowAttributes("style").globally()
      .allowAttributes("src").matching(imgSrcPolicy).onElements("img")
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

  inline def isXssUnSafe(input: String): Boolean = !isXssSafe(input)

  /**
   *
   * Default HTML normalizer function to be used in isXssSafe comparisons.
   * @param input
   * @return
   */
  def defaultHtmlNormalizer(input: String): String =
    if input == null then null
    else
      // basic entity and img normalizations first
      val base =
        input
          .replace("&nbsp;", "\u00A0")
          .replaceAll("&amp;", "&")
          .replaceAll("&lt;", "<")
          .replaceAll("&gt;", ">")
          .replaceAll("&#43;", "+")
          .replaceAll("<br>", "<br />")
          .replaceAll("(?i)<img\\b([^>]*?)\\s*/?\\s*>", "<img$1 />")

      // Only normalize whitespace *after* ':' inside style attributes.
      val stylePattern = java.util.regex.Pattern.compile("(?i)(style\\s*=\\s*\")([^\"]*?)\"", java.util.regex.Pattern.CASE_INSENSITIVE)
      val m = stylePattern.matcher(base)
      val sb = new java.lang.StringBuffer()
      while m.find() do
        val prefix = m.group(1) // e.g. style="
        val body = m.group(2)
          // remove white space after ':' only
          .replaceAll(":(\\s+)", ":")
          // remove white space after ';'
          .replaceAll(";\\s+", ";")
          // remove a trailing semicolon just before the closing quote
          .replaceAll(";\\s*$", "")
        m.appendReplacement(sb, java.util.regex.Matcher.quoteReplacement(prefix + body + "\""))
      m.appendTail(sb)
      sb.toString

  /**
   *
   * @param origInput
   * @param normalizerFn
   * @return
   */
  def isXssSafe(origInput: String, normalizerFn: String => String = defaultHtmlNormalizer): Boolean =
    // Normalize/guard null and short-circuit data: URIs first
    val text = if origInput == null then "" else origInput.trim
    if text.isBlank || htmlLikeRegex.findFirstIn(text).isEmpty then true
    else
      /*
       * Sanitize and compare with original input
       * If sanitization did not modify input, it is considered safe
       * If sanitization modified input, decode sanitized and compare with input
       */
      val sanitized = sanitize(text)
      val normalizedOrigInput = normalizerFn(origInput)
      if normalizedOrigInput.equals(sanitized) then true
      else
        val decoded = Encoding.decodeHtml(sanitized, false)
        /*
         * Input is considered safe if sanitization did not modify it.
         * Use the decoded sanitized to compare with nonNbspInput.
         */
        val isSafe = normalizedOrigInput.equals(decoded)
        if !isSafe then
          htmlSanitizerLogger.warn(s"XssNotSafe\ninput:[$origInput]\nsanitized:[$sanitized]\nnormalized:[$normalizedOrigInput]\ndecoded:[$decoded]")
        isSafe


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
