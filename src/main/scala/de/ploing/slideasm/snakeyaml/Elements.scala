package de.ploing.slideasm.snakeyaml

sealed abstract case class YamlElement()
case class YamlScalar[T](value : T) extends YamlElement
case class YamlMap(map : Map[String,YamlElement]) extends YamlElement
case class YamlSeq(list : List[YamlElement]) extends YamlElement
object YamlEmpty extends YamlElement
