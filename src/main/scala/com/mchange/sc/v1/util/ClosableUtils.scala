/*
 * Distributed as part of mchange-commons-scala v0.4.0
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
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

package com.mchange.sc.v1.util;

import com.mchange.v2.reflect.ReflectUtils;
import java.lang.reflect.Method;

/**
 *  A bug with Scala's duck typing implementation
 *  allows methods of non-public classes that implement
 *  public interfaces (or override public base class methods)
 *  to get invoked directly on the non-public class' member
 *  (which is illegal and provokes an IllegalAccessException)
 *  rather than indirectly via the public parent class or
 *  interface method. The safeClose / ReflectUtils stuff
 *  is to work around that. When Scala's duck type bugs are
 *  fixed, this should be removed.
 */ 

object ClosableUtils
{
  private val emptyClassArray = new Array[Class[Any]](0);
  private val emptyObjArray   = new Array[Object](0);
  
  type Closable = { def close() : Unit };
  type Destroyable = { def destroy() : Unit };

  private def safeClose( closable : Closable ) : Unit = {
      val m = closable.getClass().getMethod("close", emptyClassArray : _* );
      val accessibleM = ReflectUtils.findInPublicScope( m );
      accessibleM.invoke( closable, emptyObjArray : _* );
  }


  def attemptClose( closable : Closable ) : Unit = 
    {
      try { if (closable != null) /* closable.close(); */ safeClose( closable ); }
      catch { case ex : Exception => ex.printStackTrace(); }
      //println("closed " + closable); new Exception("DEBUG").printStackTrace();
    }

  def attemptClose( c1 : Closable, c2 : Closable, closables : Closable* ) : Unit = 
    { attemptClose(c1); attemptClose(c2); closables.foreach( attemptClose _ ); }

  def attemptCloseAll( closables : Closable* ) : Unit = 
    { closables.foreach( attemptClose _ ); }

  def withClosable[T <: Closable, A]( factoryMethod : () => T )( op : (T) => A ) : A = {
    val rsrc : T = factoryMethod();

    try
    { op(rsrc); }
    finally
    { attemptClose( rsrc ) ; }
  }

  private class Destroyable2Closable[T <: Destroyable]( val rsrc : T )
  {
    // def close() : Unit = rsrc.destroy();
    //
    // have to work around Scala bug (Trac Ticket #2318)

    def close() : Unit = {
      val m = rsrc.getClass().getMethod("destroy", emptyClassArray : _*);
      val accessibleM = ReflectUtils.findInPublicScope( m );
      accessibleM.invoke( rsrc, emptyObjArray : _*);
    }
  }

  def withDestroyable[T <: Destroyable]( factoryMethod : () => T )( op : (T) => Unit ) : Unit =
    withClosable( () => new Destroyable2Closable[T]( factoryMethod() ) ){ d2c => op( d2c.rsrc ) };

/* TOO CUMBERSOME

  def withFourClosables[A <: Closable, B <: Closable, C <: Closable, D <: Closable](factoryA : () => A , 
										    factoryB : (A) => B,
										    factoryC : (A,B) => C,
										    factoryD : (A,B,C) => D) ( op : (Tuple4[A,B,C,D]) => Unit ) {
    withClosable( factoryA ) {
      a =>
	withClosable( () => factoryB(a) ) {
	  b =>
	    withClosable( () => factoryC(a,b) ) {
	      c => 
		withClosable( () => factoryD(a,b,c) ) {
		  d =>
		    op( Tuple4(a, b, c, d ) ); 
		}
	    }
	}
    }
  }
*/
}
