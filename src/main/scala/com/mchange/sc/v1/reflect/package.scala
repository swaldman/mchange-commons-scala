package com.mchange.sc.v1;

import scala.reflect.runtime.universe._;

package object reflect {

  // Stealing a trick from http://stackoverflow.com/questions/11628379/how-to-know-if-an-object-is-an-instance-of-a-typetags-type
  def instanceToType[T : TypeTag]( instance : T ) = typeOf[T]

}
