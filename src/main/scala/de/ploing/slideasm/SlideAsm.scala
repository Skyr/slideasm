package de.ploing.slideasm

import processor.{NOpProcessor, MarkdownProcessor}
import scala.util.{Success, Failure}
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
import grizzled.slf4j.Logging


class SlideAsm(cfg : SlideAsm.CmdParams) extends Logging {
  def determineRawFormatProcessor(slideDir : File) : (File, FormatProcessor) = {
    val mdFile = new File(slideDir + File.separator + "slide.md")
    val htmlFile = new File(slideDir + File.separator + "slide.html")
    if (mdFile.exists) {
      (mdFile, MarkdownProcessor)
    } else if (htmlFile.exists) {
      (htmlFile, NOpProcessor)
    } else {
      SlideAsm.exit("No slide file in " + slideDir + "found")
    }
  }


  def renderTemplate(html : String, properties : Map[String,YamlElement]) = {
    new StringTemplate(html)
      .setAttributes(properties.mapValues(SlideAsm.flattenProperties(_)))
      .toString
  }


  def getStringProperty(key : String, properties : Map[String,YamlElement]) : String = {
    properties.get(key) match {
      case Some(YamlScalar(v : String)) =>
        v
      case None =>
        SlideAsm.exit("Required property " + key + " not found")
      case _ =>
        SlideAsm.exit("Property " + key + " must be a string")
    }
  }


  def wrapInTemplate(html : String, properties : Map[String,YamlElement]) = {
    val templateDirName = getStringProperty("template",properties)
    val templateName = getStringProperty("slidefile", properties)
    val templateDir = SlideAsm.findDirInDirs(templateDirName, cfg.libDirs) match {
      case Some(f) => f
      case _ => SlideAsm.exit("Template directory " + templateDirName + " not found")
    }
    val templateFile = new File(templateDir + File.separator + (if (templateName.equals("index.html"))
      "index.html" else
      "_templates" + File.separator + templateName + ".html"))
    if (templateFile.exists()) {
      val source = scala.io.Source.fromFile(templateFile)
      val template = source.mkString
      source.close()
      renderTemplate(template, properties ++ Map(("body" -> YamlScalar(html))))
    } else {
      SlideAsm.exit("Unable to find template file " + templateName + " of template " + templateDirName)
    }
  }


  /**
   * Read a file and its corresponding yaml metadata.
   * Metadata is read from a sidecar file (if existant), otherwise extracted as yaml front matter.
   *
   * @param file
   * @param sidecarFile
   * @return
   */
  def readFileWithMetadata(file : File, sidecarFile : File) = {
    val (slideMetadata, rawData) = if (sidecarFile.exists) {
      debug("  reading sidecar yaml")
      (SnakeYaml.parse(sidecarFile), new BufferedSource(new FileInputStream(file)).getLines)
    } else {
      debug("  reading front matter yaml")
      val frontMatter = SnakeYaml.parseFrontMatter(file)
      val mainData = SnakeYaml.skipOverFrontMatter(new FileInputStream(file))
      frontMatter match {
        case YamlScalar(_) if (!mainData.hasNext) =>
          // Strong indication that no front matter was present
          debug("  no front matter found")
          (YamlMap(Map()), new BufferedSource(new FileInputStream(file)).getLines)
        case _ =>
          (frontMatter, mainData)
      }
    }
    slideMetadata match {
      case YamlMap(m) =>
        (m, rawData)
      case _ =>
        SlideAsm.exit("Metadata in " + sidecarFile + " must be a map")
    }
  }


  def processSlideFile(slideName : String, inheritedProperties : Map[String,YamlElement], slideNum : Int) = {
    val slideDir : File = SlideAsm.findDirInDirs(slideName, cfg.libDirs) match {
      case None =>
        SlideAsm.exit("Slide directory " + slideName + " not found")
      case Some(f) =>
        f
    }
    info("Slide " + slideName)
    val metadataFile = new File(slideDir + File.separator + "slide.yaml")
    val (slideFile, slideProcessor) = determineRawFormatProcessor(slideDir)
    val (slideMetadata, rawData) = readFileWithMetadata(slideFile, metadataFile)
    val convertedHtml = slideProcessor.convertToHtml(rawData)
    trace("  Metadata: " + slideMetadata)
    trace("  Html (via " + slideProcessor.getClass.getSimpleName + "): " + convertedHtml)
    val slideProperties = inheritedProperties ++ slideMetadata +
      ("slideid" -> YamlScalar(f"slide$slideNum%03d")) +
      ("slidenum" -> YamlScalar(s"$slideNum"))
    val renderedTemplate = renderTemplate(convertedHtml, slideProperties)
    val xhtml = (SlideAsm.parseHtmltoXHtml(renderedTemplate) \ "body")(0).child.mkString.trim
    trace("  Rendered template: " + xhtml)
    // Process content: Rewrite image URLs, add to copy list
    // TODO
    // Wrap result in enclosing template "slidefile" (if any)
    if (inheritedProperties.contains("template") && inheritedProperties.contains("slidefile")) {
      trace("  Wrapped slide: " + wrapInTemplate(xhtml.toString, slideProperties))
    }
  }


  /**
   * Process yaml sequence describing a slide set. The elements must be:
   * <ul>
   *   <li>String elements - a name referencing a slide directory
   *   <li>A map containing either a key "include" or a key "slides".<br>
   *   "include" maps to a yaml file to be included.
   *   "slides" maps to a yaml sequence again parsed with this method.
   *   Additional keys will be added to the current properties map.
   * </ul>
   * @param slides
   * @param properties
   * @param file
   * @param firstSlideNum start index for slide numbering
   * @return the number of slides generated
   */
  def processAssemblyFileSlideSection(slides : YamlSeq, properties : Map[String,YamlElement], file : File, firstSlideNum : Int) : Int = {
    val uninheritedProperties = List("wrapfile")
    var slideNum = firstSlideNum
    // Process slide entries
    for (el <- slides.list) el match {
      case YamlScalar(v : String) =>
        processSlideFile(v, properties, slideNum)
        slideNum = slideNum + 1
      case YamlMap(m) =>
        if (!m.contains("include") && !m.contains("slides")) {
          SlideAsm.exit("Missing include/slides element in assembly file " + file)
        }
        m.get("include") match {
          case None =>
          case Some(YamlScalar(filename : String)) =>
            val incFile = {
              val incFile = SlideAsm.findFileInDirs(filename, cfg.libDirs)
              if (incFile.isEmpty) {
                SlideAsm.exit("Included assembly file " + filename + " not found")
              }
              incFile.get
            }
            info("Begin include " + incFile)
            val slideCount = processAssemblyFile(incFile, properties -- uninheritedProperties ++ m, slideNum)
            slideNum = slideNum + slideCount
            info("End include " + incFile)
          case el =>
            SlideAsm.exit("Illegal element " + el + " in assembly file " + file)
        }
        m.get("slides") match {
          case None =>
          case Some(seq : YamlSeq) =>
            info("Begin subsection")
            val slideCount = processAssemblyFileSlideSection(seq, properties -- uninheritedProperties ++ m, file, slideNum)
            slideNum = slideNum + slideCount
            info("End subsection")
          case el =>
            SlideAsm.exit("Illegal element " + el + " in assembly file " + file)
        }
      case el =>
        SlideAsm.exit("Illegal element " + el + " in assembly file " + file)
    }
    // Wrap result in enclosing template "wrapfile" (if any)
    // TODO
    // Return slide count
    slideNum - firstSlideNum
  }


  /**
   *
   * @param file
   * @param inheritedProperties
   * @param firstSlideNum start index for slide numbering
   * @return the number of slides generated
   */
  def processAssemblyFile(file : File, inheritedProperties : Map[String,YamlElement], firstSlideNum : Int) : Int = {
    // Parse assembly file
    val assemblyFile = AssemblyFile.parse(file) match {
        case Failure(ex) =>
          SlideAsm.exit(ex.getMessage)
        case Success(data) =>
          data
      }
    val properties = inheritedProperties ++ assemblyFile.properties.map
    trace("Properties of " + file + ": " + properties)
    // Execute slide section
    processAssemblyFileSlideSection(assemblyFile.slides, properties, file, firstSlideNum)
  }


  def processMainAssemblyFile() : Unit = {
    // Determine slide template name
    val mainAssemblyFile = AssemblyFile.parse(cfg.assemblyFile.get) match {
      case Failure(ex) =>
        SlideAsm.exit(ex.getMessage)
      case Success(data) =>
        data
    }
    val templateName = getStringProperty("template", mainAssemblyFile.properties.map)
    // Find slide template
    val templateDir = SlideAsm.findDirInDirs(templateName, cfg.libDirs) match {
      case None =>
        SlideAsm.exit(s"Template directory $templateName not found")
      case Some(dir) =>
        dir
    }
    // Get default properties from template
    val templateMainFile = new File(templateDir + File.separator + "index.html")
    val templateMetadataFile = new File(templateDir + File.separator + "template.yaml")
    val (defaultProperties, _) = readFileWithMetadata(templateMainFile, templateMetadataFile)
    // Validate required properties are present
    // TODO
    // Do acutal processing
    processAssemblyFile(cfg.assemblyFile.get, defaultProperties, 1)
    // Wrap result in main template
    // TODO
  }
}


object SlideAsm {
  case class CmdParams(
                        assemblyFile : Option[File] = None,
                        libDirs : List[Path] = List(),
                        outDir : Option[Path] = None)

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
      if (config.libDirs.length==0) {
        exit("No library directory given!")
      }
      // Everything is ok, start processing main assembly file
      val processor = new SlideAsm(config)
      processor.processMainAssemblyFile()
      // Copy template data to destination directory
      // TODO
      // Copy collected files (files that were referenced in the slide)
      // TODO
    } getOrElse {
      sys.exit(1)
    }
  }
}
