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

package com.mchange.sc.v2.collection.immutable;

import scala.collection._

import java.util.{Arrays,Random}

import com.mchange.lang.ByteUtils

// TODO: Abstract away commonalities in factories...
object ImmutableArraySeq {
  def apply[A]( source : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) = new ImmutableArraySeq[A]( source.clone() )( atag );

  /**
   * Be sure you know WTF you are doing, that nothing else mutates this array.
   */  
  def createNoCopy[A]( source : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) = new ImmutableArraySeq( source )( atag );

  abstract class Abstract[A] protected ( private val inner : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) extends immutable.IndexedSeq[A] {
    override def apply( i : scala.Int ) : A = inner.apply(i);

    override def iterator : Iterator[A] = inner.iterator;

    override def length : scala.Int = inner.length;

    override def toArray[B >: A](implicit btag : scala.reflect.ClassTag[B]) : Array[B] = {
      try {
        if (atag.runtimeClass == btag.runtimeClass) {
          inner.clone().asInstanceOf[Array[B]]
        } else {
          val src = inner
          val len = src.length;
          val out = btag.newArray( len );
          Array.copy( src, 0, out, 0, len );
          out
        }
      } catch {
        case e : ArrayStoreException => { // if we ask for a primitive array as an Any (rare)
          Array[B]( inner : _* )          // this formulation boxes
        }
      }
    }

    override def copyToArray[B >: A](xs: Array[B], start: scala.Int, len: scala.Int): Unit = {
      val lenToCopy = math.min( xs.length - start, len )
      Array.copy( inner, 0, xs, start, lenToCopy )
    }
  }

  object Byte {
    def apply( source : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) = new ImmutableArraySeq.Byte( source.clone() )( atag );

    /**
     * Be sure you know WTF you are doing, that nothing else can possibly mutate this array.
     */  
    def createNoCopy( source : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) = new ImmutableArraySeq.Byte( source )( atag );

    def random( len : scala.Int )( random : Random ) = {
      val tmp = Array.ofDim[scala.Byte]( len )
      random.nextBytes( tmp )
      apply( tmp )
    }
  }
  final class Byte private ( private val byteInner : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) extends Abstract[scala.Byte]( byteInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Byte => Arrays.equals( this.byteInner, other.byteInner );
        case whatever                       => super.equals( whatever );
      }
    }

    lazy val asSignedBigInteger : java.math.BigInteger = new java.math.BigInteger( byteInner );
    lazy val asSignedBigInt     : BigInt               = BigInt( asSignedBigInteger );

    lazy val asUnsignedBigInteger : java.math.BigInteger = new java.math.BigInteger( 1, byteInner );
    lazy val asUnsignedBigInt     : BigInt               = BigInt( asUnsignedBigInteger );

    override def toString() : String = s"ImmutableArraySeq.Byte(0x${ByteUtils.toLowercaseHexAscii( byteInner )})"
  }

  object Double {
    def apply( source : Array[scala.Double] )( implicit atag : scala.reflect.ClassTag[scala.Double] ) = new ImmutableArraySeq.Double( source.clone() )( atag );

    /**
     * Be sure you know WTF you are doing, that nothing else can possibly mutate this array.
     */  
    def createNoCopy( source : Array[scala.Double] )( implicit atag : scala.reflect.ClassTag[scala.Double] ) = new ImmutableArraySeq.Double( source )( atag );
  }
  final class Double private ( private val doubleInner : Array[scala.Double] )( implicit atag : scala.reflect.ClassTag[scala.Double] ) extends Abstract[scala.Double]( doubleInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Double => Arrays.equals( this.doubleInner, other.doubleInner );
        case whatever                         => super.equals( whatever );
      }
    }
  }

  object Int {
    def apply( source : Array[scala.Int] )( implicit atag : scala.reflect.ClassTag[scala.Int] ) = new ImmutableArraySeq.Int( source.clone() )( atag );

    /**
     * Be sure you know WTF you are doing, that nothing else mutates this array.
     */  
    def createNoCopy( source : Array[scala.Int] )( implicit atag : scala.reflect.ClassTag[scala.Int] ) = new ImmutableArraySeq.Int( source )( atag );
  }
  final class Int private ( private val intInner : Array[scala.Int] )( implicit atag : scala.reflect.ClassTag[scala.Int] ) extends Abstract[scala.Int]( intInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Int => Arrays.equals( this.intInner, other.intInner );
        case whatever                      => super.equals( whatever );
      }
    }
  }

  object Long {
    def apply( source : Array[scala.Long] )( implicit atag : scala.reflect.ClassTag[scala.Long] ) = new ImmutableArraySeq.Long( source.clone() )( atag );

    /**
     * Be sure you know WTF you are doing, that nothing else mutates this array.
     */  
    def createNoCopy( source : Array[scala.Long] )( implicit atag : scala.reflect.ClassTag[scala.Long] ) = new ImmutableArraySeq.Long( source )( atag );
  }
  final class Long private ( private val longInner : Array[scala.Long] )( implicit atag : scala.reflect.ClassTag[scala.Long] ) extends Abstract[scala.Long]( longInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Long => Arrays.equals( this.longInner, other.longInner );
        case whatever                       => super.equals( whatever );
      }
    }
  }
}
final class ImmutableArraySeq[A] private ( inner : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) extends ImmutableArraySeq.Abstract[A]( inner ) {
  require( !atag.runtimeClass.isPrimitive, s"Please use an implementation of ImmutableArraySeq.Abstract specialized for ${atag.runtimeClass.getName}." );
}



