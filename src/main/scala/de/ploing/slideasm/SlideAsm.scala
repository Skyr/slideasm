package de.ploing.slideasm

import grizzled.config.Configuration
import scala.xml.XML
import java.nio.file.Paths
import java.nio.file.Files
import org.pegdown.PegDownProcessor
import org.pegdown.Extensions
import org.jsoup.Jsoup
import java.io.File
import java.nio.file.Path
import scala.xml.Elem
import scopt.immutable.OptionParser
import java.nio.file.FileSystems


// Note: Look at yaml for metadata http://alvinalexander.com/scala/scala-yaml-parser-parsing-examples-snakeyaml-objects

object SlideAsm {
  case class CmdParams(assemblyFile : File = null, libDirs : List[Path] = List())

  def loadJSoupXml(path : Path) : Elem = {
    val jsoupDoc = Jsoup.parse(path.toFile, "UTF-8", "")
    XML.loadString(jsoupDoc.outerHtml)
  }

  def main(args: Array[String]): Unit = {
    println("SlideAsm - the html5 slide assembler")

    val parser = new OptionParser[CmdParams]("SlideAsm", "1.0") {
      def options = Seq(
        opt("l", "libdir", "slide library base directory") { (d: String, c: CmdParams) =>
          val path = FileSystems.getDefault().getPath(d)
          c.copy(libDirs = path :: c.libDirs) 
        },
        arg("<file>", "main assembly file") { (f: String, c: CmdParams) => 
          val file = new File(f)
          if (!file.canRead()) {
            println("Unable to read " + f)
            System.exit(1)
          }
          c.copy(assemblyFile = file) 
        }
      )
    }

    parser.parse(args, CmdParams()) map { config =>
      if (config.assemblyFile==null) {
        println("No assembly file given!")
        System.exit(1)
      }
      if (config.libDirs.length==0) {
        println("No library directory given!")
        System.exit(1)
      }
    } getOrElse {
      System.exit(1)
    }


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
    val template = loadJSoupXml(templateName)

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
            loadJSoupXml(slideHtmlFile) \\ "article"
          }
          println(slide)
        })
      }
    })
  }
}
