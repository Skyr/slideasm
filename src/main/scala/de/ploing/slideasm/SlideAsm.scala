package de.ploing.slideasm

import java.io.File
import grizzled.config.Configuration


object SlideAsm {
  def main(args: Array[String]): Unit = {
    println("SlideAsm - the html5 slide assembler")
    if (args.size!=1) {
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
    config.sectionNames.foreach(s => println(s))
  }
}