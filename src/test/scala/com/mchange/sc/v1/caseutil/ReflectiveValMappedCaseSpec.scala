package com.mchange.sc.v1.caseutil;

import scala.reflect.runtime.universe._;
import org.specs2.mutable._;

import com.mchange.sc.v1.reflect._;

// Note that for now, ValMappedCase only works with top-level classes
object Test extends CompanionOfReflectiveValMappedCase[Test] {
  def tType : Type = typeOf[Test];
}
case class Test(val str : String, val map : Map[String,String], val l : Long) extends ReflectiveValMappedCase{
  def tType : Type = Test.tType;
}

class ReflectiveValMappedCaseSpec extends Specification { 
  val testTest = Test("Hello", Map("Goodbye" -> "Then"), 1L);
  val testTestMap = Map[String,Any]( "str" -> "Hello", "map" -> Map("Goodbye" -> "Then"), "l" -> 1L);

  "A ReflectiveValMappedCase" should { 
    "extract via toMap to an appropriate Map[String,Any]" in { 
      testTest.toMap mustEqual testTestMap
    }
  }

  "An appropriate Map" should {
    "parse into a correct ReflectiveValMappedCase" in {
      testTest mustEqual Test.fromMap( testTestMap )
    }
  }
}

