package de.ploing.slideasm

import snakeyaml.{SnakeYaml, YamlSeq, YamlMap}
import java.io.File
import org.yaml.snakeyaml.error.YAMLException
import java.util.NoSuchElementException


case class AssemblyFile(properties : YamlMap, slides : YamlSeq)


object AssemblyFile {
  def parse(file : File) : Option[AssemblyFile] = {
    try {
      val result = SnakeYaml.parseAll(file)
      (result.head, result.tail.head) match {
        case (p : YamlMap, s : YamlSeq) =>
          new Some(AssemblyFile(p,s))
        case _ =>
          println("Assembly file " + file + " must consist of a map and a sequence")
          None
      }
    } catch {
      case e : YAMLException =>
        println("Error parsing assembly file " + file + ": " + e.getMessage)
        None
      case e : NoSuchElementException =>
        println("Assembly file " + file + " must contain of exactly two yaml structures")
        None
    }
  }
}
