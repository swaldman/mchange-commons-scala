package com.mchange.sc.v1.mappable;

import scala.reflect.runtime.universe._;
import com.mchange.sc.v1.reflect._;

import org.specs2.mutable._;

import com.mchange.sc.v1.decode._;

// Note that for now, Mappable only works with top-level classes
object DecodableSingletonTest extends DecodableSingleton;

class DecodableSingletonSpec extends Specification { 

  val mapRep = Map[String,Any]( 
    ".staticFactoryClassName" -> "com.mchange.sc.v1.mappable.DecodableSingletonTest",
    ".decoderClass" -> classOf[DecodableMappableDecoder].getName
  )

  "A DecodableSingleton" should { 
    "extract via toMap to an appropriate Map[String,Any] (which should contain all bindings plus a '.staticFactoryClassName' and '.decoderClass' extra)" in { 
      DecodableSingletonTest.toMap must havePairs(mapRep.toSeq : _*) //the export map might contain extra fields
    }
  }

  "An exported Map" should {
    "parse into the singleton module" in {
      decode( mapRep ) eq DecodableSingletonTest
    }
  }

}

