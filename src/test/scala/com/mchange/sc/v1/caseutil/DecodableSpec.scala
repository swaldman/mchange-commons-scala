package com.mchange.sc.v1.caseutil;

import scala.reflect.runtime.universe._;
import org.specs2.mutable._;

import com.mchange.sc.v1.reflect._;

import com.mchange.v3.decode._;

// Note that for now, ValMappedCase only works with top-level classes
object DecodableTest extends CompanionOfReflectiveValMappedCase[DecodableTest] {
  def tType : Type = typeOf[DecodableTest];
}
case class DecodableTest(val str : String, val map : Map[String,String], val l : Long) extends ReflectiveValMappedCase with Decodable {
  def tType : Type = DecodableTest.tType;
}

class DecodableSpec extends Specification { 
  val testTest = DecodableTest("Hello", Map("Goodbye" -> "Then"), 1L);
  val testTestMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L);
  val testTestAugmentedMap = 
    Map[String,Any]( 
      "str" -> "Hello", 
      "map" -> Map("Goodbye" -> "Then"), 
      "l" -> 1L, 
      ".className" -> classOf[DecodableTest].getName,
      ".decoderClass" -> classOf[DecodableValMappedCaseDecoder].getName
    );

  "A Decodable" should { 
    "extract via toMap to an appropriate Map[String,Any] (which should contain all bindings plus a '.className' and '.decoderClass' extra)" in { 
      testTest.toMap must havePairs(testTestAugmentedMap.toSeq : _*) //the export map might contain extra fields
    }
  }

  "An exported Map" should {
    "parse into a correct the correct class and value via the ClassNameExporting companion object" in {
      val decoder = Class.forName(testTestAugmentedMap(".decoderClass").asInstanceOf[String]).newInstance.asInstanceOf[Decoder]; 
      decoder.decode( testTestAugmentedMap ) mustEqual testTest
    }
  }
}

