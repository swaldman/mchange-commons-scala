package com.mchange.sc.v2

import scala.io.Codec

import java.nio.ByteBuffer

package object json {
  case class Segregated( clean : String, controlCharacters : String ) {
    def escapedControlCharacterList : List[String] = controlCharacters.map( c => f"\\u${c}%04x" ).toList
    def escapedControlCharacters    : String       = escapedControlCharacterList.foldLeft("")(_ + _)
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

  private def stringFromCodePoints( cps : Array[Int] ) = new String( cps, 0, cps.length )
}
