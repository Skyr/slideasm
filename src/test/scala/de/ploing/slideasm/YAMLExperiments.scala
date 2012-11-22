package de.ploing.slideasm

import scala.reflect.BeanProperty
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor


class Metadata {
  @BeanProperty var template : String = null
}


object YAMLExperiments {
    val text1 = """
template: slide
style: plain
    """
      
  def main(args: Array[String]): Unit = {
    /*
    val yaml = new Yaml(new Constructor(classOf[Metadata]))
    val e = yaml.load(text1).asInstanceOf[Metadata]
    println(e)
    */
    val yaml = new Yaml
    val e = yaml.load(text1).asInstanceOf[java.util.Map[String, Object]]
    println(e)
  }
}