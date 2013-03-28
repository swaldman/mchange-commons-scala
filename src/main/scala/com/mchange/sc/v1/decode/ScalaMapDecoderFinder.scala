package com.mchange.sc.v1.decode;

import com.mchange.v3.decode._;

class ScalaMapDecoderFinder extends DecoderFinder {

  @throws[CannotDecodeException]
  def decoderClassName( encoded : Object ) : String = {
    try {
      encoded match {
	case map : Map[String,Any] => map.getOrElse( DecodeUtils.DECODER_CLASS_DOT_KEY, map.getOrElse( DecodeUtils.DECODER_CLASS_NO_DOT_KEY, null ) ).asInstanceOf[String];
	case _ => null;
      }
    } catch {
      case e : Exception => throw new CannotDecodeException("An Exception occurred while trying to decode a Scala map.", e);
    }
  }

}
