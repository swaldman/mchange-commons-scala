package com.mchange.sc.v1;

import java.util.concurrent._;
import scala.reflect.runtime.universe._;

import com.mchange.sc.v1.util.concurrent.RestrictToThreadInvoker;


package object reflect {

  // Stealing a trick from 
  // http://stackoverflow.com/questions/11628379/how-to-know-if-an-object-is-an-instance-of-a-typetags-type
  def instanceToCompileTimeType[T : TypeTag]( instance : T ) = typeOf[T]

  def instanceToCompileTimeTypeTag[T : TypeTag] = implicitly[TypeTag[T]];


  /**
   * Scala reflection is not-at-all Thread-safe as of now [2013-03-23]
   *
   * Until that's fixed, we'll set up an "invoker" in which all reflection-related operations
   * should be run. @see com.mchange.sc.v1.util.concurrent.RestrictToThreadInvoker
   *
   * Any operations touching scala.reflect.* or com.mchange.sc.v1.reflect.* should be executed here. 
   */ 
  lazy val ReflectionInvoker = new RestrictToThreadInvoker( "com.mchange.sc.v1.ReflectionInvoker" );
}
