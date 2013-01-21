package de.ploing.slideasm

import processor.{NOpProcessor, MarkdownProcessor}
import scala.xml.XML
import java.nio.file.Files
import org.jsoup.Jsoup
import java.io.{FileInputStream, File}
import java.nio.file.Path
import scala.xml.Elem
import java.nio.file.FileSystems
import snakeyaml._
import scopt.immutable.OptionParser
import snakeyaml.YamlMap
import snakeyaml.YamlScalar
import scala.Some
import io.BufferedSource
import org.clapper.scalasti.StringTemplate


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

  def parseHtmltoXHtml(html : String) : Elem = {
    val jsoupDoc = Jsoup.parse(html)
    XML.loadString(jsoupDoc.outerHtml)
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


  def flattenProperties(props : YamlElement) : Any = {
    props match {
      case YamlScalar(s) =>
        s.toString
      case YamlMap(m) =>
        m.mapValues(flattenProperties(_))
      case YamlSeq(l) =>
        l.map(flattenProperties(_))
      case _ =>
        ""
    }
  }

  def renderTemplate(html : String, cfg : CmdParams, properties : Map[String,YamlElement]) = {
    new StringTemplate(html)
      .setAttributes(properties.mapValues(flattenProperties(_)))
      .toString
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
    val (slideMetadata, rawData) = {
      val (slideMetadata, rawData) = if (metadataFile.exists) {
        println("  reading sidecar yaml")
        (SnakeYaml.parse(metadataFile), new BufferedSource(new FileInputStream(slideFile)).getLines)
      } else {
        println("  reading front matter yaml")
        (SnakeYaml.parseFrontMatter(slideFile), SnakeYaml.skipOverFrontMatter(new FileInputStream(slideFile)))
      }
      slideMetadata match {
        case YamlMap(m) =>
          (m, rawData)
        case _ =>
          exit("Metadata of slide " + slideName + " must be a map")
      }
    }
    val convertedHtml = slideProcessor.convertToHtml(rawData)
    println("  Metadata: " + slideMetadata)
    println("  Html (via " + slideProcessor.getClass.getSimpleName + "): " + convertedHtml)
    val renderedTemplate = renderTemplate(convertedHtml, cfg, inheritedProperties ++ slideMetadata)
    val xhtml = parseHtmltoXHtml(renderedTemplate)
    println("  Rendered template: " + xhtml)
    // Process content: Rewrite image URLs, add to copy list
    // TODO
    // Wrap result in enclosing template "slidefile" (if any)
    // TODO
  }


  def processAssemblyFileSlideSection(slides : YamlSeq, properties : Map[String,YamlElement], cfg : CmdParams, file : File) : Unit = {
    // Process slide entries
    for (el <- slides.list) el match {
      case YamlScalar(v : String) =>
        processSlideFile(v, cfg, properties)
      case YamlMap(m) =>
        if (!m.contains("include") && !m.contains("slides")) {
          exit("Missing include/slides element in assembly file " + file)
        }
        m.get("include") match {
          case None =>
          case Some(YamlScalar(filename : String)) =>
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
        m.get("slides") match {
          case None =>
          case Some(seq : YamlSeq) =>
            println("Begin subsection")
            processAssemblyFileSlideSection(seq, properties ++ m, cfg, file)
            println("End subsection")
          case el =>
            exit("Illegal element " + el + " in assembly file " + file)
        }
      case el =>
        exit("Illegal element " + el + " in assembly file " + file)
    }
    // Wrap result in enclosing template "wrapfile" (if any)
    // TODO
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
    // Execute slide section
    processAssemblyFileSlideSection(assemblyFile.slides, properties, cfg, file)
  }


  def processMainAssemblyFile(cfg : CmdParams) : Unit = {
    // Get default properties from template
    val defaultProperties : Map[String, YamlElement] = Map()  // TODO
    // Validate required properties are present
    // TODO
    // Do acutal processing
    processAssemblyFile(cfg.assemblyFile.get, cfg, defaultProperties)
    // Wrap result in main template
    // TODO
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
      // Everything is ok, start processing main assembly file
      processMainAssemblyFile(config)
      // Copy template data to destination directory
      // TODO
      // Copy collected files (files that were referenced in the slide)
      // TODO
    } getOrElse {
      sys.exit(1)
    }
  }
}
