package de.ploing.slideasm.snakeyaml

sealed case class BaseElement()
case class YamlElement[T](el : T) extends BaseElement
case class YamlMap(map : Map[String,BaseElement]) extends BaseElement
case class YamlList(list : List[BaseElement]) extends BaseElement
