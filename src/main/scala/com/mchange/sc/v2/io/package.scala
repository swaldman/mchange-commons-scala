package com.mchange.sc.v2;

import lang.borrow

import scala.io.Codec

import java.io._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

package object io {
  def withPrintWriter[T]( file : File, bufferLen : Int )( op : PrintWriter => T )( implicit codec : Codec ) : T = {
    borrow( new PrintWriter( new OutputStreamWriter( new BufferedOutputStream( new FileOutputStream( file ), bufferLen ), codec.charSet ) ) )( op )
  }
}
