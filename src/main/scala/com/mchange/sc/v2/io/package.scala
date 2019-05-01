/*
 * Distributed as part of mchange-commons-scala v0.4.9
 *
 * Copyright (C) 2019 Machinery For Change, LLC
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php
 *
 */

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

  implicit class RichInputStream( val is : InputStream ) extends AnyVal {
    def remainingToByteArray : Array[Byte] = {
      val bis : BufferedInputStream = {
        is match {
          case b : BufferedInputStream => b
          case o                       => new BufferedInputStream(o)
        }
      }
      try {
        val baos = new ByteArrayOutputStream()
        var c = bis.read()
        while ( c >= 0 ) {
          baos.write(c)
          c = bis.read()
        }
        baos.toByteArray
      }
      finally {
        bis.close()
      }
    }
    def remainingToByteSeq : immutable.Seq[Byte] = {
      ImmutableArraySeq.Byte.createNoCopy( remainingToByteArray )
    }
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
