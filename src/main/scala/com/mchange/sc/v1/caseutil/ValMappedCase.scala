package com.mchange.sc.v1.caseutil;

/**
 * T should be the type of the case class.
 */ 
trait CompanionOfValMappedCase[T <: ValMappedCase] {
  def fromMap( map : Map[String,Any] ) : T; 
}

trait ValMappedCase {
  def toMap : Map[String,Any];
}


