package com.mchange.sc.v1.mappable;

trait AnnotatedMappable extends Mappable {
  final def toMap : Map[String,Any] = _toMap ++ extraBindings;

  def _toMap : Map[String,Any];
  
  def extraBindings : Iterable[ ( String, Any ) ] = Map.empty[String,Any];
}

