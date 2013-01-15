package de.ploing.slideasm


trait FormatProcessor {
  def convertToHtml(in : TraversableOnce[String]) : String
}
