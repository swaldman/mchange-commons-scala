package com.mchange.sc.v2

import scala.io.Codec

import java.nio.ByteBuffer

package object json {
  case class Segregated( clean : String, controlCharacters : String ) {
    def escapedControlCharacterList : List[String] = controlCharacters.map( c => f"\\u${c}%04x" ).toList
    def escapedControlCharacters    : String       = escapedControlCharacterList.foldLeft("")(_ + _)

    def cleanBytes( codec : Codec = Codec.UTF8 ) : Array[Byte] = clean.getBytes( codec.charSet )
  }

  def segregateControlCharacters( input : String ) : Segregated = {
    val codePoints = input.codePoints.toArray
    val ( controlCodePoints, regCodePoints ) = codePoints.partition( Character.isISOControl )
    Segregated( stringFromCodePoints( regCodePoints ), stringFromCodePoints( controlCodePoints ) )
  }

  def segregateControlCharacters( input : Array[Byte], codec : Codec ) : Segregated = {
    segregateControlCharacters( new String( input, 0, input.length, codec.charSet ) )
  }

  def segregateControlCharacters( input : Array[Byte] ) : Segregated = {
    segregateControlCharacters( input, Codec.UTF8 )
  }

  def segregateControlCharacters( input : ByteBuffer, codec : Codec ) : Segregated = {
    segregateControlCharacters( codec.decoder.decode( input ).toString )
  }

  def segregateControlCharacters( input : ByteBuffer ) : Segregated = {
    segregateControlCharacters( input, Codec.UTF8 )
  }

  def removeNulTermination( input : String ) : String = {
    if ( notNulTerminated( input ) ) {
      input
    } else {
      input.substring( 0, input.length - 1 )
    }
  }

  def removeNulTermination( input : Array[Byte], codec : Codec = Codec.UTF8 ) : Array[Byte] = {
    codec match {
      case Codec.UTF8 | Codec.ISO8859 => {
        val len = input.length
        if ( len == 0 || input(len-1) != 0 ) {
          input
        }
        else {
          val out = Array.ofDim[Byte]( len - 1 )
          Array.copy( input, 0, out, 0, len - 1 )
          out
        }
      }
      case _ => {
        val cs = codec.charSet
        removeNulTermination( new String( input, 0, input.length, cs ) ).getBytes( cs )
      }
    }
  }

  private def notNulTerminated( input : String ) : Boolean = {
    val len = input.length
    len == 0 || input(len - 1) != 0 // || ( len > 1 && Character.isSurrogatePair( input(len-2), input(len-1) ) ) XXX Not necessary, NUL cannot be a low surrogate!
  }

  private def stringFromCodePoints( cps : Array[Int] ) = new String( cps, 0, cps.length )
}
