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
import snakeyaml.{YamlScalar, YamlSeq, YamlMap, SnakeYaml}
import java.util.NoSuchElementException
import org.yaml.snakeyaml.error.YAMLException


object SlideAsm {
  case class CmdParams(assemblyFile : Option[File] = None, libDirs : List[Path] = List(), outDir : Option[Path] = None)

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


  def processAssemblyFile(file : File, cfg : CmdParams) : Unit = {
    // Parse assembly file
    val assemblyFile = {
      val data = AssemblyFile.parse(file)
      if (data.isEmpty) {
        System.exit(1)
      }
      data.get
    }
    println(assemblyFile.properties)
    println(assemblyFile.slides)
    for (el <-assemblyFile.slides.list) el match {
      case YamlScalar(v : String) =>
        println("Slide " + v)
      case YamlMap(m) =>
        if (!m.contains("include")) {
          println("Missing include element in assembly file " + file)
          System.exit(1)
        }
        m.get("include").get match {
          case YamlScalar(filename : String) =>
            val incFile = {
              val incFile = findFileInDirs(filename, cfg.libDirs)
              if (incFile.isEmpty) {
                println("Included assembly file " + filename + " not found")
                System.exit(1)
              }
              incFile.get
            }
            println("Begin include " + m.get("include"))
            processAssemblyFile(incFile, cfg)
            println("End include " + m.get("include"))
          case el =>
            println("Illegal element " + el + " in assembly file " + file)
            System.exit(1)
        }
      case el =>
        println("Illegal element " + el + " in assembly file " + file)
        System.exit(1)
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
            println("Library directory " + d + " does not exist!")
            System.exit(1)
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
              println("File " + f + " not found")
              System.exit(1)
              c
            case Some(file) =>
              if (!file.canRead()) {
                println("Unable to read " + f)
                System.exit(1)
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
        println("No assembly file given!")
        System.exit(1)
      }
      if (config.libDirs.length==0) {
        println("No library directory given!")
        System.exit(1)
      }
      // Everything is ok, get to work
      processAssemblyFile(config.assemblyFile.get, config)
    } getOrElse {
      System.exit(1)
    }
  }
}
