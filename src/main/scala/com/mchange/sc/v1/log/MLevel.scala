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

package com.mchange.sc.v1.log;

import language.implicitConversions;

object MLevel {

  implicit def toJavaLevel( scalaLevel : MLevel ) : com.mchange.v2.log.MLevel = scalaLevel._level;

  private[log] def forInner( inner : com.mchange.v2.log.MLevel ) : MLevel = inner match {
    case com.mchange.v2.log.MLevel.ALL => ALL
    case com.mchange.v2.log.MLevel.CONFIG => CONFIG
    case com.mchange.v2.log.MLevel.FINE => FINE
    case com.mchange.v2.log.MLevel.FINER => FINER
    case com.mchange.v2.log.MLevel.FINEST => FINEST
    case com.mchange.v2.log.MLevel.INFO => INFO
    case com.mchange.v2.log.MLevel.OFF => OFF
    case com.mchange.v2.log.MLevel.SEVERE => SEVERE
    case com.mchange.v2.log.MLevel.WARNING => WARNING
  }

  case object ALL extends MLevel( com.mchange.v2.log.MLevel.ALL );
  case object CONFIG extends MLevel( com.mchange.v2.log.MLevel.CONFIG );
  case object FINE extends MLevel( com.mchange.v2.log.MLevel.FINE );
  case object FINER extends MLevel( com.mchange.v2.log.MLevel.FINER );
  case object FINEST extends MLevel( com.mchange.v2.log.MLevel.FINEST );
  case object INFO extends MLevel( com.mchange.v2.log.MLevel.INFO );
  case object OFF extends MLevel( com.mchange.v2.log.MLevel.OFF );
  case object SEVERE extends MLevel( com.mchange.v2.log.MLevel.SEVERE );
  case object WARNING extends MLevel( com.mchange.v2.log.MLevel.WARNING );
}

sealed abstract class MLevel ( private[log] val _level : com.mchange.v2.log.MLevel ) {
  private[log] def doIf[T]( op : com.mchange.v2.log.MLevel => T )(implicit logger : MLogger) : Unit = if ( logger.inner.isLoggable( _level ) ) op( _level )

  def log( message : =>String )( implicit logger : MLogger ) = doIf( logger.inner.log( _, message ) );
  def log( message : =>String, error : =>Throwable )( implicit logger : MLogger ) = doIf( logger.inner.log( _, message, error ) );
  def logFormat( message : =>String, params : Seq[Any] )( implicit logger : MLogger ) = doIf( logger.inner.log( _, message, params.toArray ) );
}



