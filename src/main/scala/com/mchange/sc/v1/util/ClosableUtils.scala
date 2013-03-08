/*
 * Distributed as part of mchange-commons-scala
 *
 * Copyright (C) 2013 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1, as 
 * published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this software; see the file LICENSE.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
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
