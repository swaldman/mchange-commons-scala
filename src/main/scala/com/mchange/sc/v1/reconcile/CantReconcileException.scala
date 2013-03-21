/*
 * Distributed as part of mchange-commons-scala v0.4.0
 *
 * Copyright (C) 2013 Machinery For Change, Inc.
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

package com.mchange.sc.v1.reconcile;

class CantReconcileException(message : String, cause : Throwable) extends Exception( message, cause )
{
  def this( message : String ) = this( message, null );
  def this( cause : Throwable ) = this( null, cause );
  def this() = this( null, null );
}

object CantReconcileException
{
    def ouch(irreconcilable1 : Any, irreconcilable2 : Any) : Nothing =
    { throw new CantReconcileException( "Both fields have distinct values, cannot reconcile -- %s and %s.".format(irreconcilable1, irreconcilable2) ); }
}

