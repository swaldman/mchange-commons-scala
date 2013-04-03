package com.mchange.sc.v1.mappable;

import com.mchange.sc.v1.decode._;

trait CompanionOfDecodable[T <: Decodable] extends CompanionOfMappable[T] with DecodeCoercer[T];

trait Decodable extends ClassNameExporting { 
  override def extraBindings : Iterable[ ( String, Any ) ] = super.extraBindings ++ Map( DecoderClassKey -> classOf[DecodableMappableDecoder].getName );
}

