package com.mchange.sc.v1;

import com.mchange.v3.decode._;

package object decode {
  val DecoderClassKey = DecodeUtils.DECODER_CLASS_DOT_KEY; // ".decoderClass"

  def decode( obj : Any ) = DecodeUtils.decode( obj );
}
