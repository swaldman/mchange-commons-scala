package com.mchange.sc.v1.caseutil;

object Decodable {
  val DecoderClassKey = ".decoderClass";
}

trait Decodable extends ClassNameExporting { self : ReflectiveValMappedCase =>
  override def extraBindings : Iterable[ ( String, Any ) ] = super.extraBindings ++ Map( Decodable.DecoderClassKey -> classOf[DecodableValMappedCaseDecoder].getName );
}

