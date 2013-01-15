package de.ploing.slideasm

import grizzled.config.Configuration
import processor.{NOpProcessor, MarkdownProcessor}
import scala.xml.XML
import java.nio.file.Paths
import java.nio.file.Files
import org.pegdown.PegDownProcessor
import org.pegdown.Extensions
import org.jsoup.Jsoup
import java.io.{FileInputStream, File}
import java.nio.file.Path
import scala.xml.Elem
import scopt.immutable.OptionParser
import java.nio.file.FileSystems
import snakeyaml._
import java.util.NoSuchElementException
import org.yaml.snakeyaml.error.YAMLException
import scopt.immutable.OptionParser
import snakeyaml.YamlMap
import snakeyaml.YamlScalar
import scala.Some
import io.BufferedSource


object SlideAsm {
  case class CmdParams(assemblyFile : Option[File] = None, libDirs : List[Path] = List(), outDir : Option[Path] = None)

  def exit(msg : String) = {
    println(msg)
    sys.exit(1)
  }


  def loadJSoupXml(path : Path) : Elem = {
    val jsoupDoc = Jsoup.parse(path.toFile, "UTF-8", "")
    XML.loadString(jsoupDoc.outerHtml)
  }
  
  def grizzledProcessAssemblyFile(cfg : CmdParams) = {
    val mainFile = cfg.assemblyFile.get
    val config = new Configuration()
    config.load(scala.io.Source.fromFile(mainFile), true)

    val slideBaseDir = mainFile.getParent
    val templateName = Paths.get(mainFile.getParent(), config.get("main", "template").get, "template.html")
    val template = loadJSoupXml(templateName)

    val sections = config.get("main", "sections")
    if (!sections.isDefined) {
      exit("sections in main missing")
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


  def findFileInDirs(fileName : String, libDirs : List[Path]) : Option[File] = {
    libDirs map { path =>
      new File(path + path.getFileSystem.getSeparator + fileName)
    } find { file =>
      file.exists && file.isFile
    }
  }


  def findDirInDirs(dirName : String, libDirs : List[Path]) : Option[File] = {
    libDirs map { path =>
      new File(path + path.getFileSystem.getSeparator + dirName)
    } find { file =>
      file.exists && file.isDirectory
    }
  }


  def determineRawFormatProcessor(slideDir : File) : (File, FormatProcessor) = {
    val mdFile = new File(slideDir + File.separator + "slide.md")
    val htmlFile = new File(slideDir + File.separator + "slide.html")
    if (mdFile.exists) {
      (mdFile, MarkdownProcessor)
    } else if (htmlFile.exists) {
      (htmlFile, NOpProcessor)
    } else {
      exit("No slide file in " + slideDir + "found")
    }
  }


  def processSlideFile(slideName : String, cfg : CmdParams, inheritedProperties : Map[String,YamlElement]) = {
    val slideDir : File = findDirInDirs(slideName, cfg.libDirs) match {
      case None =>
        exit("Slide directory " + slideName + " not found")
      case Some(f) =>
        f
    }
    println("Slide " + slideName)
    val metadataFile = new File(slideDir + File.separator + "slide.yaml")
    val (slideFile, slideProcessor) = determineRawFormatProcessor(slideDir)
    val (slideMetadata, rawData) = if (metadataFile.exists) {
      println("  reading sidecar yaml")
      (SnakeYaml.parse(metadataFile), new BufferedSource(new FileInputStream(slideFile)).getLines)
    } else {
      println("  reading front matter yaml")
      (SnakeYaml.parseFrontMatter(slideFile), SnakeYaml.skipOverFrontMatter(new FileInputStream(slideFile)))
    }
    val convertedHtml = slideProcessor.convertToHtml(rawData)
    println("  Metadata: " + slideMetadata)
    // println("  Body: " + rawData.toList.mkString(" "))
    println("  Html (via " + slideProcessor.getClass.getSimpleName + "): " + convertedHtml)
  }


  def processAssemblyFile(file : File, cfg : CmdParams, inheritedProperties : Map[String,YamlElement]) : Unit = {
    // Parse assembly file
    val assemblyFile = {
      val data = AssemblyFile.parse(file)
      if (data.isEmpty) {
        sys.exit(1)
      }
      data.get
    }
    val properties = inheritedProperties ++ assemblyFile.properties.map
    println("Properties of " + file + ": " + properties)
    for (el <-assemblyFile.slides.list) el match {
      case YamlScalar(v : String) =>
        processSlideFile(v, cfg, properties)
      case YamlMap(m) =>
        if (!m.contains("include")) {
          exit("Missing include element in assembly file " + file)
        }
        m.get("include").get match {
          case YamlScalar(filename : String) =>
            val incFile = {
              val incFile = findFileInDirs(filename, cfg.libDirs)
              if (incFile.isEmpty) {
                exit("Included assembly file " + filename + " not found")
              }
              incFile.get
            }
            println("Begin include " + incFile)
            processAssemblyFile(incFile, cfg, properties ++ m)
            println("End include " + incFile)
          case el =>
            exit("Illegal element " + el + " in assembly file " + file)
        }
      case el =>
        exit("Illegal element " + el + " in assembly file " + file)
    }
  }


  def main(args: Array[String]): Unit = {
    println("SlideAsm - the html5 slide assembler")

    // Setup scopt command line parsing
    val parser = new OptionParser[CmdParams]("SlideAsm", "1.0") {
      def options = Seq(
        opt("l", "libdir", "<directory>", "slide library base directory") { (d: String, c: CmdParams) =>
          val path = FileSystems.getDefault().getPath(d)
          if (!Files.exists(path)) {
            exit("Library directory " + d + " does not exist!")
          }
          c.copy(libDirs = c.libDirs ::: List(path)) 
        },
        opt("o", "outdir", "output directory") { (d: String, c: CmdParams) =>
          val path = FileSystems.getDefault().getPath(d)
          c.copy(outDir = Some(path)) 
        },
        arg("<file>", "main assembly file") { (f: String, c: CmdParams) =>
          val fp = FileSystems.getDefault().getPath(f).toAbsolutePath
          val libDirs = fp.getParent :: c.libDirs
          findFileInDirs(fp.getFileName.toString, libDirs) match {
            case None =>
              exit("File " + f + " not found")
            case Some(file) =>
              if (!file.canRead()) {
                exit("Unable to read " + f)
              }
              // Prepend path of assembly file as first
              c.copy(assemblyFile = Some(file), libDirs = libDirs)
          }
        }
      )
    }

    // Execute command line parser
    parser.parse(args, CmdParams()) map { config =>
      // Parameters parsed successfully, do last validations
      if (config.assemblyFile==None) {
        exit("No assembly file given!")
      }
      if (config.libDirs.length==0) {
        exit("No library directory given!")
      }
      // Everything is ok, get to work
      processAssemblyFile(config.assemblyFile.get, config, Map())
    } getOrElse {
      sys.exit(1)
    }
  }
}
