package scala.meta.internal.pc

object HoverMarkup {

  // VSCode trims the message to ~100000, thus messing the markdown for very long messages.
  // Number based on experiments from 29.09.2023.
  private val MaxHoverBodyLength = 50000

  /**
   * Render the textDocument/hover result into markdown.
   *
   * @param expressionType The type of the expression over the cursor, for example "List[Int]".
   * @param symbolSignature The signature of the symbol over the cursor, for example
   *                        "def map[B](fn: A => B): Option[B]"
   * @param docstring The Scaladoc/Javadoc string for the symbol.
   */
  def apply(
      expressionType: String,
      optSymbolSignature: Option[String],
      docstring: String,
      forceExpressionType: Boolean = false,
      contextInfo: List[String] = Nil
  ): String = {
    val markdown = new StringBuilder()
    if (contextInfo.nonEmpty) {
      markdown
        .append("```scala\n")
        .append(contextInfo.mkString("\n"))
        .append("\n```\n\n")
    }
    if (forceExpressionType || optSymbolSignature.isEmpty) {
      markdown
        .append(
          if (optSymbolSignature.isDefined) "**Expression type**:\n" else ""
        )
        .append("```scala\n")
        .append(expressionType)
        .append("\n```\n")
    }
    optSymbolSignature.foreach { symbolSignature =>
      if (symbolSignature.nonEmpty) {
        markdown
          .append(if (forceExpressionType) "**Symbol signature**:\n" else "")
          .append("```scala\n")
          .append(symbolSignature)
          .append("\n```")
      }
    }
    if (docstring.nonEmpty)
      markdown
        .append("\n")
        .append(docstring)
    markdown.toString()
  }

  private def trimBody(body: String) =
    if (body.length() <= MaxHoverBodyLength) body
    else body.take(MaxHoverBodyLength) + "..."

  def apply(body: String): String = {
    s"""|```scala
        |${trimBody(body)}
        |```""".stripMargin
  }

  def javaHoverMarkup(body: String): String = {
    s"""|```java
        |${trimBody(body)}
        |```""".stripMargin
  }

  def javaHoverMarkup(
      expressionType: String,
      symbolSignature: String,
      docstring: String,
      forceExpressionType: Boolean = false
  ): String = {
    val markdown = new StringBuilder()
    if (forceExpressionType) {
      markdown
        .append("**Expression type**:\n")
        .append("```java\n")
        .append(expressionType)
        .append("\n```\n")
    }
    if (symbolSignature.nonEmpty) {
      markdown
        .append(if (forceExpressionType) "**Symbol signature**:\n" else "")
        .append("```java\n")
        .append(symbolSignature)
        .append("\n```")
    }
    if (docstring.nonEmpty)
      markdown
        .append("\n")
        .append(docstring)
    markdown.toString()
  }
}
