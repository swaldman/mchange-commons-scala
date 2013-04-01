package com.mchange.sc.v1.mappable;

import com.mchange.v3.decode._;

class DecodableMappableDecoder extends Decoder {

  @throws[CannotDecodeException]
  def decode( obj : Object ) : Object = {
    try {
      obj match {
	case map : Map[String,Any] => { //unchecked. hmmm...
	  val maybe = ClassNameExporting.maybeFromMap( map );
	  maybe match {
	    case Some( vmc : Mappable ) => vmc;
	    case Some( unk ) => throw new CannotDecodeException( "Object of unexpected type: %s".format( unk ) );
	    case None => throw new CannotDecodeException( "Could not decode source map: %s".format( map ) );
	  }
	}
	case _ => throw new CannotDecodeException( "Unexpected source type: %s".format( obj ) )
      }
    } catch {
      case e : Exception => throw new CannotDecodeException( "An Exception occurred while trying to decode source object: %s".format( obj ), e )
    }
  }

}
