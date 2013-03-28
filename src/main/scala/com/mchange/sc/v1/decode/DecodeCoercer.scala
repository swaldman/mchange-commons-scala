package com.mchange.sc.v1.decode;

import com.mchange.sc.v1.reflect._;
import scala.reflect.runtime.universe._;

import com.mchange.sc.v1.log._;
import MLevel._;

object DecodeCoercer {
  implicit val logger = MLogger( this );
}

trait DecodeCoercer[T] {
  import DecodeCoercer._;

  def typeTag : TypeTag[T]

  def coerce( obj : Any) : Option[T] = {
    ReflectionInvoker.await{
      try {
	Some( decode( obj ).asInstanceOf[T] )
      } catch {
	case e : Exception => FINE.log( "Failed to coerce %s to %s.".format( obj, typeTag.tpe ), e );
	None
      }
    }
  }
}
