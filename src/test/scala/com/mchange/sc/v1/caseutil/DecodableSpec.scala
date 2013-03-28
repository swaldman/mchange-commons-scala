package com.mchange.sc.v1.caseutil;

import scala.reflect.runtime.universe._;
import com.mchange.sc.v1.reflect._;

import org.specs2.mutable._;

import com.mchange.sc.v1.decode._;

// Note that for now, ValMappedCase only works with top-level classes
object DecodableTest extends CompanionOfDecodable[DecodableTest]{
  def typeTag = compileTimeTypeTag[DecodableTest];
}

case class DecodableTest(val str : String, val map : Map[String,String], val l : Long) extends Decodable {
  def tType : Type = DecodableTest.typeTag.tpe;
}

class DecodableSpec extends Specification { 
  val testTest = DecodableTest("Hello", Map("Goodbye" -> "Then"), 1L);
  println( testTest.toMap );
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
    "parse into a correct the correct class and value via the decode function" in {
      //val decoder = Class.forName(testTestAugmentedMap(".decoderClass").asInstanceOf[String]).newInstance.asInstanceOf[Decoder]; 
      //decoder.decode( testTestAugmentedMap ) mustEqual testTest
      decode( testTestAugmentedMap ) mustEqual testTest
    }
  }


  "A Decodable's companion object" should {
    "be able to 'coerce' an appropriate Map into a Decodable" in {
      DecodableTest.coerce( testTestAugmentedMap ) mustEqual Some( testTest )
    }
    "be fail to 'coerce' an inappropriate object into a Decodable" in {
      DecodableTest.coerce( DecodableTest ) mustEqual None
    }
  }
}

