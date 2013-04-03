package com.mchange.sc.v1.decode;

import com.mchange.sc.v1.log._;
import MLevel._;

object DecodeCoercer {
  implicit val logger = MLogger( this );
}

trait DecodeCoercer[T] {
  import DecodeCoercer._;

  def coerce( obj : Any) : Option[T] = {
    try {
      Some( decode( obj ).asInstanceOf[T] )
    } catch {
      case e : Exception => FINE.log( "Failed to coerce %s to desired type.".format( obj ), e );
	None
    }
  }
}
