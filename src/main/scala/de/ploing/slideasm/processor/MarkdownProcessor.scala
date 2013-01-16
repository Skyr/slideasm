package de.ploing.slideasm.processor

import de.ploing.slideasm.FormatProcessor
import org.pegdown.{Extensions, PegDownProcessor}
import scala.Predef.String


object MarkdownProcessor extends FormatProcessor {
  val mdParser = new PegDownProcessor(Extensions.ALL)

  def convertToHtml(in : TraversableOnce[String]) : String = {
    val mdString = in.toList.mkString("\n")
    mdParser.markdownToHtml(mdString)
  }
}
