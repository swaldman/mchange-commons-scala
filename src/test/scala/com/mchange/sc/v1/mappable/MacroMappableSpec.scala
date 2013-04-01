package com.mchange.sc.v1.mappable;

import org.specs2.Specification;

object MacroMappableSpec {
  val Str : String         = "Hallo!";
  val Col : java.awt.Color = java.awt.Color.blue;
  val Num : Int            = 9;
  val M   : Map[Any,Any]   = Map("Scooter"->8, 11->6.7d);

  case class TestCase(  val str : String, val col : java.awt.Color, val num : Int, m : Map[Any,Any] ) extends Mappable {
    def toMap : Map[String,Any] = scalamacro.extractMap( this );
  }

  object TestCase extends CompanionOfMappable[TestCase] {
    def fromMap( map : Map[String,Any] ) : TestCase = scalamacro.constructFromMap[TestCase]( map ); 
  }

  val defaultTestCase = TestCase( Str, Col, Num, M );
  val defaultMap      : Map[String,Any] = Map[String,Any]( "str" -> Str, "col" -> Col, "num" -> Num, "m" -> M );
  
}

class MacroMappableSpec extends Specification {
  import MacroMappableSpec._

  def is = {
    "Testing a Mappable case class implemented via macros."                                        ^
    "defaultTestCase object should extract to the expected Map"                 ! extractsToMap    ^
    "defaultMap should construct into defaultTestCase"                          ! constructsToCase ;
  }

  def extractsToMap    = defaultTestCase.toMap mustEqual defaultMap;
  def constructsToCase = TestCase.fromMap( defaultMap ) mustEqual defaultTestCase;
}
