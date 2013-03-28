package com.mchange.sc.v1.caseutil;

import com.mchange.sc.v1.decode._;

trait CompanionOfDecodable[T <: Decodable] extends CompanionOfReflectiveValMappedCase[T] with DecodeCoercer[T];

trait Decodable extends ClassNameExporting { 
  override def extraBindings : Iterable[ ( String, Any ) ] = super.extraBindings ++ Map( DecoderClassKey -> classOf[DecodableValMappedCaseDecoder].getName );
}

