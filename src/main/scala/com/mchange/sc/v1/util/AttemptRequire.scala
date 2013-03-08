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

import com.mchange.v2.log._;

object AttemptRequire {
  
  private class RequirementException( cause : Exception ) extends Exception( cause );

  def require( op : => Unit ) : Unit = {
    try {
      op;
    } catch {
      case e : Exception => throw new RequirementException(e);
    }
  }

  def attempt[T]( swallow : Boolean, logLevel : MLevel, opName : String, dflt : T)(op : => T)(implicit logger : MLogger) : T = {
    try {
      op;
    } catch {
      case re : RequirementException => throw re.getCause();
      case e : Exception => {
	if ( swallow ) {
	  if (logger.isLoggable( logLevel ) )
	    logger.log(logLevel, opName + " failed, but failure was ignored." + (if (dflt != ()) " Returning default value."), e);
	  dflt;
	} else {
	  throw e;
	}
      }
    }
  }

  def attempt( swallow : Boolean, logLevel : MLevel, opName : String)(op : => Unit)(implicit logger : MLogger) : Unit = attempt( swallow, logLevel, opName, ())(op)(logger);

  def attempt[T]( logLevel : MLevel, opName : String, dflt : T)(op : => T )(implicit logger : MLogger) : T = attempt( true, logLevel, opName, dflt )( op )( logger );

  def attempt( logLevel : MLevel, opName : String)(op : => Unit )(implicit logger : MLogger) : Unit = attempt( logLevel, opName, () )( op )( logger );

  def attempt[T]( dflt : T )( op : => T ) : T = attempt( MLevel.OFF, null, dflt )( op )( null );

  //def attempt( op : => Unit ) : Unit = attempt( () )( op );
}
