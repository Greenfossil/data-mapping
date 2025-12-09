package com.greenfossil.data.mapping

import com.fasterxml.jackson.databind.ObjectMapper
import org.owasp.html.{HtmlPolicyBuilder, PolicyFactory}

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
      .allowUrlProtocols("http", "https", "mailto")
      // Do not allow any 'on*' event attributes or script-like protocols
      .toFactory()

  def sanitize(input: String): String =
    if input == null then null
    else policy.sanitize(input)

  private val jsonMapper = new ObjectMapper()

  /** Returns true when sanitizer modified input (heuristic detection of unsafe content).
   * Avoid false positives for JSON by detecting and validating JSON before sanitizing.
   */
  def containsUnsafe(input: String): Boolean =
    if input == null then false
    else
      val trimmed = input.trim

      // If there are no obvious HTML markers, treat as non-HTML.
      if !trimmed.contains('<') && !trimmed.contains('&') then
        // If it looks like JSON, try to parse it; valid JSON is not HTML.
        if trimmed.startsWith("{") || trimmed.startsWith("[") then
          try
            jsonMapper.readTree(trimmed)
            false
          catch
            case _: Exception => false // not valid JSON but also no HTML markers -> treat as safe
        else
          false
      else
        val cleaned = sanitize(input)
        val origNorm = trimmed
        val cleanedNorm = Option(cleaned).map(_.trim).getOrElse("")
        origNorm != cleanedNorm

end HtmlSanitizer
