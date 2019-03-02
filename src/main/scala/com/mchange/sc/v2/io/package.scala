package com.mchange.sc.v2;

import lang.{borrow, borrowExplicit} // borrowExplicit is a workaround for when Scala upgrades cause the normal borrow to become misinterpreted

import scala.io.{Codec,Source}

import scala.collection._

import java.io._
import java.nio.file.{Paths, Files, StandardOpenOption}

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

package object io {
  val K128 = 128 * 1024

  def withPrintWriter[T]( file : File, bufferLen : Int )( op : PrintWriter => T )( implicit codec : Codec ) : T = {
    borrow( new PrintWriter( new OutputStreamWriter( new BufferedOutputStream( new FileOutputStream( file ), bufferLen ), codec.charSet ) ) )( op )
  }

  implicit class RichFile( val file : File ) extends AnyVal {
    def contentsAsString( bufferSize : Int, codec : Codec ) : String = {
      borrowExplicit( Source.fromFile( file, bufferSize )( codec ) )( _.close )( _.mkString )
    }
    def contentsAsString( codec : Codec ) : String = this.contentsAsString( K128, codec )

    def contentsAsString : String = this.contentsAsString( Codec.default )

    def contentsAsByteArray : Array[Byte] = Files.readAllBytes( file.toPath )

    def contentsAsByteSeq : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( contentsAsByteArray )

    def replaceContents( bytes : Seq[Byte] ) : Unit = {
      replaceContents( bytes.toArray )
    }
    def replaceContents( bytes : Array[Byte] ) : Unit = {
      Files.write( file.toPath, bytes )
    }
    def replaceContents( string : String, codec : Codec = Codec.default ) : Unit = {
      Files.write( file.toPath, string.getBytes( codec.charSet ) )
    }
    def appendContents( bytes : Seq[Byte] ) : Unit = {
      appendContents( bytes.toArray )
    }
    def appendContents( bytes : Array[Byte] ) : Unit = {
      Files.write( file.toPath, bytes, StandardOpenOption.APPEND )
    }
    def appendContents( string : String, codec : Codec = Codec.default ) : Unit = {
      Files.write( file.toPath, string.getBytes( codec.charSet ), StandardOpenOption.APPEND )
    }
  }
}
