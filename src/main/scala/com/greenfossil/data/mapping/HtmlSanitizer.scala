package com.greenfossil.data.mapping

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.parser.Tag
import org.jsoup.safety.Safelist
import org.owasp.html.{AttributePolicy, Encoding, HtmlPolicyBuilder}
import org.slf4j.LoggerFactory

import java.util.Locale
import scala.jdk.CollectionConverters.*

/**
 * Centralized OWASP HTML sanitizer utility.
 * - `sanitize` returns cleaned HTML (or null for null input).
 * - `containsUnsafe` returns true when the sanitizer removed or altered input (indicating unsafe content).
 */
object HtmlSanitizer:

  private val htmlSanitizerLogger = LoggerFactory.getLogger("data-mapping.HtmlSanitizer")

  // base64-encoded image data: URI payloads (the only data: form allowed for href and img src)
  private val dataImageUriRegex = "^data:image/(png|jpe?g|gif|webp);base64,[A-Za-z0-9+/=]+$"

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
                if (v.matches(dataImageUriRegex)) v else null
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
                if (v.matches(dataImageUriRegex)) v else null
              case _ => null
            }
          } catch {
            case _: Exception => null
          }
      }
    }

  @deprecated("to be removed, use sanitize instead")
  private val policy =
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

  @deprecated("use sanitize")
  def sanitize2(input: String): String =
    if input == null then null
    else policy.sanitize(input)

  /*
  * Remotely an HTML tag
  */
  val htmlLikeRegex =
    """(?i)<\s*/?\s*[a-z][a-z0-9]*\b(?:[^>]*>?|$)""".r

  // Structural elements produced by the HTML parser itself; never candidates for unwrapping.
  private val structuralTags = Set("#root", "html", "head", "body")

  /** True when an unknown element can only have come from a plain-text `<...>` token.
    *
    * Two parser-level facts distinguish text from markup here:
    *  - attributes are only created when the tokenizer saw a `name=value` pair inside the tag,
    *    which plain prose never produces;
    *  - a tag name containing `<` means the input was malformed markup (e.g. `<scr<script>ipt>`),
    *    because a single text token stops at the first `<`.
    * Unknown elements that fail either test are left in place, so the sanitize-vs-original
    * comparison still flags them as modified (fail closed).
    */
  private def isPlainTextTokenElement(el: Element): Boolean =
    el.attributes().isEmpty && !el.tagName.contains('<')

  /** Removes elements whose tag is not a real HTML element by unwrapping them (children kept).
    *
    * Plain text such as an Outlook-style quote — `From: Alice Tan <alice.tan@example.com>` — is not
    * markup, but Jsoup's parser reads `<alice.tan@example.com>` as the start tag
    * `alice.tan@example.com` and therefore creates an element for it. Such a bogus tag must not
    * count as markup, otherwise sanitization (which drops unknown tags) appears to change the
    * input and legitimate text is reported as an XSS attempt. Jsoup is the source of truth for
    * "is this a real HTML element" (`Tag.isKnownTag`), so no pattern-matching is needed here.
    *
    * Only text-like unknown elements are unwrapped (see `isPlainTextTokenElement`), and children
    * are kept so a genuine known tag nested inside an unknown one is preserved.
    *
    * `Element.unwrap()` is deliberately not used: it removes one child at a time and re-indexes the
    * parent's sibling list on every call, which is quadratic when many unknown siblings share a
    * parent (e.g. `"<x>" * 100_000`, reachable through an unbounded `optional(text)` field).
    * Instead each affected parent's child list is rebuilt once, preserving order; nodes outside
    * the candidate set are left untouched. Linear overall.
    */
  private def unwrapUnknownElements(doc: Document): Document =
    val candidates = doc.getAllElements.asScala.toList
      .filterNot(el => structuralTags.contains(el.tagName))
      .filterNot(el => Tag.isKnownTag(el.tagName))
      .filter(isPlainTextTokenElement)
    candidates.groupBy(_.parent()).toList
      // deepest parents first, so nested unknown tags are flattened before their own parent is
      .sortBy { case (parent, _) => -parent.parents.asScala.size }
      .foreach { case (parent, elements) =>
        val unwrap = elements.toSet // Node equality is identity, so this is a pure membership test
        val flattened = parent.childNodes.asScala.toList.flatMap {
          case el: Element if unwrap.contains(el) => el.childNodes.asScala.toList
          case other => List(other)
        }
        parent.empty()
        parent.insertChildren(0, flattened.asJava)
      }
    doc

  /** Collapses the formatting whitespace that the original and sanitized serializations disagree
    * about, so it cannot decide the XSS verdict.
    *
    * `sanitize` (via `Jsoup.clean`) trims a trailing newline or space that the parser kept around a
    * dropped tag — before `</body>`, `</p>`, `</div>` or `</td>`. Without this, `From: A <a@b.co>`
    * followed by a newline compares unequal to its cleaned form even though nothing meaningful was
    * removed. Whitespace adjacent to a tag boundary is formatting, not content.
    */
  private def normaliseHtml(html: String): String =
    html.replaceAll("\\s+", " ")
      .replaceAll(">\\s+", ">")
      .replaceAll("\\s+<", "<")
      .trim

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


  // Allowed image MIME types for data: URLs
  private val allowedDataImages = Seq(
    "data:image/png",
    "data:image/jpeg",
    "data:image/jpg",
    "data:image/gif",
    "data:image/webp"
  )

  /** Strips the URL control characters (tab/CR/LF) that a browser URL parser ignores.
    *
    * Without this, obfuscated targets such as `//evil.com` split by a tab or `\//evil.com`
    * survive the protocol-relative check because the browser (not the raw string) sees `//evil.com`.
    */
  private def stripHrefControlChars(href: String): String =
    href.filterNot(ch => ch == '\t' || ch == '\r' || ch == '\n').trim

  /** Validates a link target that the safelist preserved verbatim.
    *
    * `preserveRelativeLinks` keeps relative references untouched (so `/we/index`, `#anchor` and
    * `www.example.com` survive), which also means Jsoup no longer normalizes the value
    * before the protocol check. This guard closes the two gaps that leaves:
    *  - scheme-relative targets (`//host/...`, including `\\host/...` and `/\host/...` once
    *    normalized the way a browser does) bypass the http/https protocol whitelist;
    *  - a dangerous scheme hidden behind leading junk (`\"javascript:...`) is treated as a plain
    *    relative reference.
    */
  private def isPreservedHrefSafe(href: String): Boolean =
    // browser URL parsing: strip tab/CR/LF, then map '\' onto '/'
    val stripped = stripHrefControlChars(href)
    val normalized = stripped.replace('\\', '/')
    if normalized.startsWith("//") then false
    else
      // Probe from the first alphanumeric char, ignoring leading quotes/backslashes/whitespace/junk,
      // but never skip past a leading '/' or '#': those make the value a relative reference, which
      // can never carry a scheme (so `#data:section` or `/help/javascript:guide` stay safe).
      val probe = stripped.dropWhile(ch => !ch.isLetterOrDigit && ch != '/' && ch != '#').toLowerCase(Locale.ENGLISH)
      if probe.startsWith("javascript:") || probe.startsWith("vbscript:") || probe.startsWith("file:") then false
      else if probe.startsWith("data:") then probe.matches(dataImageUriRegex)
      else true

  private val safelist: Safelist = Safelist.relaxed()
    /*
     * Preserve relative and scheme-less links (e.g. "/we/index", "www.example.com", "#anchor").
     * Without this, Jsoup resolves hrefs against an empty base URI and strips them, so every legitimate
     * relative link in an HTML email template is reported as an XSS modification. Protocol checking is
     * still enforced: only http/https/mailto/# are allowed, so javascript:/data: hrefs are still removed.
     */
    .preserveRelativeLinks(true)
    .addTags(
      "font", "span", "sub", "sup", "u", "s", "strike",
      "thead", "th", "img", "hr"
    )
    .addAttributes("img", "align", "alt", "height", "src", "title", "width", "style")
    .addProtocols("img", "src", "http", "https", "data")
    .removeTags("script", "iframe", "object", "embed", "svg")
    .addAttributes(":all", "style", "class")
    .addAttributes("font", "color", "face", "size")
    .addAttributes("th", "colspan", "rowspan")
    .addAttributes("a", "href", "target")
    .addProtocols("a", "href", "http", "https", "mailto", "#")

  private val htmlTagPattern = "(?is)<[^>]+>".r
  private val obfuscatedEventHandlerPattern = "(?i)\\bon[a-z]{2,20}[^=<>]{0,100}=".r
  private val dangerousProtocols = "(?i)(javascript|vbscript|data:text|file):".r

  def containsMaliciousPatterns(input: String): Boolean = {
    // Scan only inside HTML tags
    val tags = htmlTagPattern.findAllIn(input)
    val hasObfuscatedEvent = tags.exists: tag =>
      obfuscatedEventHandlerPattern.findFirstIn(tag).nonEmpty
    hasObfuscatedEvent || dangerousProtocols.findFirstIn(input).nonEmpty
  }

  // Pattern to match template variables like ${variableName}
  private val templateVariablePattern = "^\\$\\{[a-zA-Z_][a-zA-Z0-9_]*\\}$".r

  /** Clean HTML and remove unsafe data: URLs */
  def sanitize(input: String): String = {
    // First, extract and preserve template variables in href before cleaning
    val inputDoc = Jsoup.parseBodyFragment(input)
    inputDoc.outputSettings().prettyPrint(false)

    val hrefTemplateVars = scala.collection.mutable.Map[String, String]()
    var counter = 0
    inputDoc.select("a[href]").asScala.foreach { anchor =>
      val href = anchor.attr("href").trim
      if (templateVariablePattern.matches(href)) {
        // Store this template variable with a numeric marker
        counter += 1
        val marker = s"TEMPLATE_VAR_$counter"
        hrefTemplateVars(marker) = href
        // Use # which is an allowed protocol
        anchor.attr("href", s"#$marker")
      }
    }

    val modifiedInput = inputDoc.body().html()
    val cleaned = Jsoup.clean(modifiedInput, safelist)
    val doc = Jsoup.parseBodyFragment(cleaned)
    doc.outputSettings().prettyPrint(false)

    // Restore template variables in href attributes
    doc.select("a[href]").asScala.foreach { anchor =>
      val href = anchor.attr("href")
      hrefTemplateVars.foreach { case (marker, templateVar) =>
        if (href == s"#$marker") {
          anchor.attr("href", templateVar)
        }
      }
    }

    // Drop hrefs kept verbatim as relative references but which still carry a disallowed scheme
    doc.select("a[href]").asScala.foreach { anchor =>
      if !isPreservedHrefSafe(anchor.attr("href")) then anchor.removeAttr("href")
    }

    // Remove unsafe data:image URLs. Relative img[src] is intentionally unsupported (only absolute
    // http(s) and image data: URLs are kept), so such images are dropped from sanitize output.
    doc.select("img[src]").asScala.foreach { img =>
      val src = img.attr("src").toLowerCase.trim

      val isSafeData = src.startsWith("data:") && allowedDataImages.exists(src.startsWith)
      val isSafeHttp = src.startsWith("http://") || src.startsWith("https://")

      // Remove if not safe
      if (!isSafeData && !isSafeHttp) {
        img.remove()
      }
    }
    // Remove unsafe styles
    doc.select("*[style]").forEach { el =>
      val style = el.attr("style").toLowerCase

      if (
        style.contains("javascript:") ||
          style.contains("expression(") ||
          style.contains("url(") && style.contains("javascript")
      ) {
        el.removeAttr("style")
      }
    }

    doc.body().html()
  }


  /** Detect XSS safely */
  def isXssSafe(input: String): Boolean = {
    // Cheap gates first: no tag-like text at all means there is nothing to sanitize.
    if input.isBlank || input.isEmpty || htmlLikeRegex.findFirstIn(input).isEmpty then true
    else if containsMaliciousPatterns(input) then
      //because Jsoup correct malform tags. any malform xss tags needs to be checked here
      htmlSanitizerLogger.warn(s"Input contains containsMaliciousPatterns: [$input]")
      false
    else
      // Parse the raw input once and run both structural scans on it: they must see the original
      // markup, before any normalization.
      val rawDoc = Jsoup.parseBodyFragment(input)

      val cleaned = sanitize(input) // this will clean and remove any tags not conforming to the policy

      // Check for any removed dangerous tags. Jsoup does not consider `svg` a "known" tag, so this
      // explicit list (rather than `Tag.isKnownTag`) is what keeps `<svg>` unsafe after unwrapping.
      val unsafeTags = Seq(
        "script", "iframe", "object", "embed", "link", "style", "svg"
      )

      val removedTagDetected = rawDoc.select(unsafeTags.mkString(",")).asScala.nonEmpty &&
        rawDoc.select(unsafeTags.mkString(",")).asScala.exists(tag => !safelist.isSafeTag(tag.tagName()))

      // Check for any removed event attributes (on the raw document, before unwrapping)
      val unsafeAttrs = rawDoc.select("*").asScala.exists { el =>
        el.attributes().asScala.exists(attr => attr.getKey.toLowerCase.startsWith("on"))
      }

      /*
       * Unwrapping unknown elements is what lets plain-text `<name@host>` tokens compare equal:
       * both sides lose the bogus tag the parser invented for the address. It must NOT be used to
       * skip the `containsMaliciousPatterns` / `unsafeAttrs` checks above — a payload such as
       * `<x@y.com onerror=alert(1)>` would otherwise compare equal and be reported as safe.
       *
       * Both documents keep the default output settings on purpose: `sanitize` returns
       * pretty-printed HTML, so serializing the two sides with different settings would report the
       * indentation it introduces as a difference. Whitespace differences around tag boundaries
       * are removed by `normaliseHtml` instead.
       */
      val originalDoc = unwrapUnknownElements(rawDoc)
      val cleanedDoc = unwrapUnknownElements(Jsoup.parseBodyFragment(cleaned))

      val isEqual = normaliseHtml(originalDoc.html()).equals(normaliseHtml(cleanedDoc.html()))
      val isSafe = isEqual && !(removedTagDetected || unsafeAttrs)
      if !isSafe then
        htmlSanitizerLogger.warn(s"XssNotSafe\ninput:[$input]\noriginalDoc:[$originalDoc]\ncleanedDoc:[$cleanedDoc]")
        htmlSanitizerLogger.warn(s"XssNotSafe\nisEqual:[$isEqual]\nisSafe:[$isSafe]\nremovedTagDetected:[$removedTagDetected]\nunsafeAttrs:[$unsafeAttrs]")
        isSafe
      else isSafe
  }

  /**
   *
   * @param origInputX
   * @param normalizerFn
   * @return
   */
  @deprecated("use isXssSafe")
  def isXssSafe2(origInput: String, normalizerFn: String => String = defaultHtmlNormalizer): Boolean =
    // Normalize/guard null and short-circuit data: URIs first
    val text = if origInput == null then "" else origInput.trim
    if text.isBlank || htmlLikeRegex.findFirstIn(text).isEmpty then true
    else
      /*
       * Sanitize and compare with original input
       * If sanitization did not modify input, it is considered safe
       * If sanitization modified input, decode sanitized and compare with input
       */
      val sanitized = sanitize2(text)
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

  /** Returns true when sanitization changes the input (heuristic detection of unsafe content).
    * HTML-free JSON and plain text are therefore treated as safe.
    */
  def containsUnsafe(input: String): Boolean =
    isXssUnSafe(input)

end HtmlSanitizer
