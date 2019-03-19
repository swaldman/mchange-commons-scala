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

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

package object lang {

  implicit lazy val logger = MLogger( "com.mchange.sc.v2.lang" );

  def attemptClose( resource : AutoCloseable, t : Throwable = null ) = {
    try { if (resource != null) resource.close(); }
    catch {
      case e : Exception => {
        if ( t != null )
          t.addSuppressed( e );
        FINE.log("Suppressed Exception on close().", e);
      }
    }
  }

  def attemptDestroy[R]( resource : R, destroy : R => Any, t : Throwable = null ) = {
    try { if (resource != null) destroy( resource ); }
    catch {
      case e : Exception => {
        if ( t != null )
          t.addSuppressed( e );
        FINE.log("Suppressed Exception on destroy.", e);
      }
    }
  }

  def borrow[R <: AutoCloseable,A]( resource : =>R )( op : R => A ) : A = {
    var throwable : Throwable = null;
    val r = resource;
    try { op( r ) }
    catch {
      case t : Throwable => {
        throwable = t;
        throw t;
      }
    }
    finally { attemptClose( r, throwable ); }
  }

  def borrow[R,A]( resource : =>R )( destroy : R => Any )( op : R => A ) : A = {
    var throwable : Throwable = null;
    val r = resource;
    try { op( r ) }
    catch {
      case t : Throwable => {
        throwable = t;
        throw t;
      }
    }
    finally { attemptDestroy( r, destroy, throwable ); }
  }
  /**
    *  When resource types become autocloseble in new versions of Scala, code that used to
    *  be compiled using the 3 arg list borrow instead become interpreted as a function call
    *  on the result of the two arg list version.
    * 
    *  To ensure consistent use of the 3-arglist version, modify the code to use borrowExplicit
    * 
    *  See, for example, the source code for com.mchange.sc.v2.io.RichFile.contentsAsString(...)
    */ 
  def borrowExplicit[R,A]( resource : =>R )( destroy : R => Any )( op : R => A ) : A = borrow[R,A]( resource )( destroy )( op )
}

