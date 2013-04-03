package com.mchange.sc.v1.mappable;

import scala.reflect.runtime.universe._;
import com.mchange.sc.v1.reflect._;

import org.specs2.mutable._;

// Note that for now, Mappable only works with top-level classes
object CNETest extends CompanionOfClassNameExporting[CNETest]{
  def fromMap( map : Map[String,Any] ) : CNETest = scalamacro.constructFromMap[CNETest]( map ); 
}

case class CNETest(val str : String, val map : Map[String,String], val l : Long) extends ClassNameExporting{
  def _toMap : Map[String,Any] = scalamacro.extractMap( this );
  val staticFactoryClassName : String = "com.mchange.sc.v1.mappable.CNETest"
}

class ClassNameExportingSpec extends Specification { 
  val testTest = CNETest("Hello", Map("Goodbye" -> "Then"), 1L);
  val testTestMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L);
  val testTestAugmentedMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L, ".staticFactoryClassName" -> classOf[CNETest].getName);

  "A ClassNameExporting" should { 
    "extract via toMap to an appropriate Map[String,Any] (which should contain all bindings plus a '.className' extra)" in { 
      testTest.toMap must havePairs(testTestAugmentedMap.toSeq : _*) //the export map might contain extra fields
    }
  }

  "An exported Map" should {
    "parse into a correct the correct class and value via the ClassNameExporting companion object" in {
      ClassNameExporting.maybeFromMap( testTestAugmentedMap ).get mustEqual testTest
    }
  }
}

