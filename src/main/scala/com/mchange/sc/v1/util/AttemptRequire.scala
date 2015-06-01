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
	    logger.log(logLevel, opName + " failed, but failure was ignored." + (if (dflt != (())) " Returning default value."), e);
	  dflt;
	} else {
	  throw e;
	}
      }
    }
  }

  def attempt( swallow : Boolean, logLevel : MLevel, opName : String)(op : => Unit)(implicit logger : MLogger) : Unit = attempt( swallow, logLevel, opName, (()))(op)(logger);

  def attempt[T]( logLevel : MLevel, opName : String, dflt : T)(op : => T )(implicit logger : MLogger) : T = attempt( true, logLevel, opName, dflt )( op )( logger );

  def attempt( logLevel : MLevel, opName : String)(op : => Unit )(implicit logger : MLogger) : Unit = attempt( logLevel, opName, () )( op )( logger );

  def attempt[T]( dflt : T )( op : => T ) : T = attempt( MLevel.OFF, null, dflt )( op )( null );

  //def attempt( op : => Unit ) : Unit = attempt( () )( op );
}
