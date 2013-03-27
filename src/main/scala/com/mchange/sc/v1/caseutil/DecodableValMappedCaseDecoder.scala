package com.mchange.sc.v1.caseutil;

import com.mchange.v3.decode._;

class DecodableValMappedCaseDecoder extends Decoder {

  @throws[CannotDecodeException]
  def decode( obj : Object ) : Object = {
    try {
      obj match {
	case map : Map[String,Any] => { //unchecked, but a ClassCastException would be caught below.
	  val maybe = ClassNameExporting.fromMap( map );
	  maybe match {
	    case Some( vmc : ValMappedCase ) => vmc;
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
