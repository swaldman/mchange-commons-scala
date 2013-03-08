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

