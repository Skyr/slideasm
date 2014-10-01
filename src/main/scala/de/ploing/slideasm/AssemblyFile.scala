package de.ploing.slideasm

import snakeyaml.{SnakeYaml, YamlSeq, YamlMap}
import java.io.File
import org.yaml.snakeyaml.error.YAMLException
import java.util.NoSuchElementException

import scala.util.{Failure, Try, Success}


case class AssemblyFile(properties : YamlMap, slides : YamlSeq)


object AssemblyFile {
  def parse(file : File) : Try[AssemblyFile] = {
    try {
      val result = SnakeYaml.parseAll(file)
      (result.head, result.tail.head) match {
        case (p : YamlMap, s : YamlSeq) =>
          new Success(AssemblyFile(p,s))
        case _ =>
          Failure(new IllegalArgumentException(s"Assembly file $file must consist of a map and a sequence"))
      }
    } catch {
      case e : YAMLException =>
        Failure(new IllegalArgumentException(s"Error parsing assembly file $file: ${e.getMessage}"))
      case e : NoSuchElementException =>
        Failure(new IllegalArgumentException(s"Assembly file $file must contain of exactly two yaml structures"))
    }
  }
}
