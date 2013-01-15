package de.ploing.slideasm.processor

import de.ploing.slideasm.FormatProcessor


object NOpProcessor extends FormatProcessor {
  def convertToHtml(in : TraversableOnce[String]) : String = {
    in.toList.mkString("\n")
  }
}
