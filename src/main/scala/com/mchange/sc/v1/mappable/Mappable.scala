package com.mchange.sc.v1.mappable;

trait CompanionOfMappable[T <: Mappable] {
  def fromMap( map : Map[String,Any] ) : T; 
}

trait Mappable {
  def toMap : Map[String,Any];
}


