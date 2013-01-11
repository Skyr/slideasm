package de.ploing.slideasm.snakeyaml

import java.io.{FileInputStream, File, InputStream}
import org.yaml.snakeyaml.Yaml
import scala.collection.JavaConverters._


object SnakeYaml {
  def convertFromSnakeYaml(obj : Object) : YamlElement = {
    obj match {
      case null =>
        YamlEmpty
      case l : java.util.List[Object] =>
        val containedElements = for (el <- l.asScala) yield convertFromSnakeYaml(el)
        new YamlSeq(containedElements.toList)
      case m : java.util.Map[String,Object] =>
        val containedMap : Map[String,YamlElement] = m.asScala.mapValues { obj =>
          convertFromSnakeYaml(obj)
        }.toMap
        new YamlMap(containedMap)
      case _ =>
        YamlScalar(obj)
    }
  }

  def parse(in : InputStream) : YamlElement = {
    val yaml = new Yaml()
    convertFromSnakeYaml(yaml.load(in))
  }

  def parse(file : File) : YamlElement = {
    parse(new FileInputStream(file))
  }
}
