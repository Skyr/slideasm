package de.ploing.slideasm

import java.io.File
import grizzled.config.Configuration
import scala.xml.XML
import java.nio.file.Paths
import java.nio.file.Files
import org.pegdown.PegDownProcessor
import org.pegdown.Extensions


object SlideAsm {
  def main(args: Array[String]): Unit = {
    println("SlideAsm - the html5 slide assembler")
    if (args.length != 1) {
      println("Exactly one parameter (assembly file) required")
      System.exit(1)
    }
    val mainFile = new File(args(0))
    if (!mainFile.canRead()) {
      println("Unable to read " + args(0))
      System.exit(1)
    }
    val config = new Configuration()
    config.load(scala.io.Source.fromFile(mainFile), true)

    val slideBaseDir = mainFile.getParent
    val templateName = Paths.get(mainFile.getParent(), config.get("main", "template").get, "template.html")
    val template = XML.loadFile(templateName.toString)

    val sections = config.get("main", "sections")
    if (!sections.isDefined) {
      println("sections in main missing")
      System.exit(1)
    }
    sections.get.split(",").foreach(section => {
      println(section)
      val slides = config.get(section, "slides")
      if (slides.isDefined) {
        slides.get.split(",").foreach(slideDir => {
          println("  " + slideDir)
          val slideMdFile = Paths.get(slideBaseDir.toString, slideDir, "slide.md")
          val slideHtmlFile = Paths.get(slideBaseDir.toString, slideDir, "slide.html")
          val slide = if (Files.isReadable(slideMdFile)) {
            val mdParser = new PegDownProcessor(Extensions.ALL)
            val mdString = new String(Files.readAllBytes(slideMdFile))
            val parsedMd = "<article>" + mdParser.markdownToHtml(mdString) + "</article>"
            XML.loadString(parsedMd)
          } else {
            XML.loadFile(slideHtmlFile.toString)
          }
        })
      }
    })
  }
}
