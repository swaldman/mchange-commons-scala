package com.mchange.sc.v1.mappable;

import scala.reflect.runtime.universe._;
import com.mchange.sc.v1.reflect._;

import org.specs2.mutable._;

import com.mchange.sc.v1.decode._;

// Note that for now, Mappable only works with top-level classes
object DecodableTest extends CompanionOfDecodable[DecodableTest]{
  def fromMap( map : Map[String,Any] ) : DecodableTest = scalamacro.constructFromMap[DecodableTest]( map ); 
}

case class DecodableTest(val str : String, val map : Map[String,String], val l : Long) extends Decodable {
  def _toMap : Map[String,Any] = scalamacro.extractMap( this );
  val staticFactoryClassName : String = "com.mchange.sc.v1.mappable.DecodableTest"
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
      ".staticFactoryClassName" -> "com.mchange.sc.v1.mappable.DecodableTest",
      ".decoderClass" -> classOf[DecodableMappableDecoder].getName
    );

  "A Decodable" should { 
    "extract via toMap to an appropriate Map[String,Any] (which should contain all bindings plus a '.staticFactoryClassName' and '.decoderClass' extra)" in { 
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

