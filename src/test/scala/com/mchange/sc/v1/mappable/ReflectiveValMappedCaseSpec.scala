package com.mchange.sc.v1.mappable;

import scala.reflect.runtime.universe._;
import com.mchange.sc.v1.reflect._;

import org.specs2.mutable._;

// Note that for now, Mappable only works with top-level classes
object Test extends CompanionOfReflectiveMappable[Test]{
  def typeTag = compileTimeTypeTag[Test];
}

case class Test(val str : String, val map : Map[String,String], val l : Long) extends ReflectiveMappable{
  def tType : Type = Test.typeTag.tpe;
}

class ReflectiveMappableSpec extends Specification { 
  val testTest = Test("Hello", Map("Goodbye" -> "Then"), 1L);
  val testTestMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L);

  "A ReflectiveMappable" should { 
    "extract via toMap to an appropriate Map[String,Any] (which should contain all bindings but might contain extras)" in { 
      testTest.toMap must havePairs(testTestMap.toSeq : _*) //the export map might contain extra fields
    }
  }

  "An appropriate Map" should {
    "parse into a correct ReflectiveMappable" in {
      testTest mustEqual Test.fromMap( testTestMap )
    }
  }
}

