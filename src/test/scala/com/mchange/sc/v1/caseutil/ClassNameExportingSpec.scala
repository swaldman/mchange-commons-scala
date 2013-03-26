package com.mchange.sc.v1.caseutil;

import scala.reflect.runtime.universe._;
import org.specs2.mutable._;

import com.mchange.sc.v1.reflect._;

// Note that for now, ValMappedCase only works with top-level classes
object CNETest extends CompanionOfReflectiveValMappedCase[CNETest] {
  def tType : Type = typeOf[CNETest];
}
case class CNETest(val str : String, val map : Map[String,String], val l : Long) extends ReflectiveValMappedCase with ClassNameExporting{
  def tType : Type = CNETest.tType;
}

class ClassNameExportingSpec extends Specification { 
  val testTest = CNETest("Hello", Map("Goodbye" -> "Then"), 1L);
  val testTestMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L);
  val testTestAugmentedMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L, ".className" -> classOf[CNETest].getName);

  "A ClassNameExporting" should { 
    "extract via toMap to an appropriate Map[String,Any] (which should contain all bindings plus a '.className' extra)" in { 
      testTest.toMap must havePairs(testTestAugmentedMap.toSeq : _*) //the export map might contain extra fields
    }
  }

  "An exported Map" should {
    "parse into a correct the correct class and value via the ClassNameExporting companion object" in {
      ClassNameExporting.fromMap( testTestAugmentedMap ).get mustEqual testTest
    }
  }
}

