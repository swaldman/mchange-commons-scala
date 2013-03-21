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

import java.util.ResourceBundle;

object MLogger {
  def apply( name : String ) : MLogger = {
    val unwrapped = com.mchange.v2.log.MLog.getLogger( name );
    new MLogger( unwrapped );
  }

  def apply( obj : Any ) : MLogger = apply ( obj.getClass.getName );
}

class MLogger( private[log] val inner : com.mchange.v2.log.MLogger ){

  import MLevel._;

  implicit val logger : MLogger = this;

  def resourceBundle : ResourceBundle = inner.getResourceBundle();
  def resourceBundleName : String = inner.getResourceBundleName();
   def level_=( level : MLevel) : Unit = inner.setLevel( level._level );
  def level : MLevel = MLevel.forInner( inner.getLevel() );
  
  def log( level : MLevel, message : =>String ) : Unit = level.doIf( inner.log( _, message ) );
  def log( level : MLevel, message : =>String, error : =>Throwable ) : Unit = level.doIf( inner.log( _, message, error ) );
  def logFormat( level : MLevel, message : =>String, params : =>Seq[Any] ) = level.doIf( inner.log( _, message, params.toArray ) );
  def logp( level : MLevel, sourceClass : =>String, sourceMethod : =>String,  message : =>String) = level.doIf( inner.logp( _, sourceClass, sourceMethod, message) );
  def logp( level : MLevel, sourceClass : =>String, sourceMethod : =>String,  message : =>String, error : =>Throwable) = level.doIf( inner.logp( _, sourceClass, sourceMethod, message, error) );
  def logpFormat( level : MLevel, sourceClass : =>String, sourceMethod : =>String,  message : =>String, params : =>Seq[Any] ) = level.doIf( inner.logp( _, sourceClass, sourceMethod, message, params.toArray) );
  def logrb( level : MLevel, sourceClass : =>String, sourceMethod : =>String, resourceBundle : =>String, message : =>String) = level.doIf( inner.logrb( _, sourceClass, sourceMethod, resourceBundle, message) );
  def logrb( level : MLevel, sourceClass : =>String, sourceMethod : =>String, resourceBundle : =>String, message : =>String, error : =>Throwable) = level.doIf( inner.logrb( _, sourceClass, sourceMethod, resourceBundle, message, error) );
  def logrbFormat( level : MLevel, sourceClass : =>String, sourceMethod : =>String, resourceBundle : =>String, message : =>String, params : =>Seq[Any] ) = level.doIf( inner.logrb( _, sourceClass, sourceMethod, resourceBundle, message, params.toArray) );
  
  def entering(sourceClass : =>String, sourceMethod : =>String) = FINER.doIf( l => inner.entering( sourceClass, sourceMethod ) );
  def entering(sourceClass : =>String, sourceMethod : =>String, params : =>Seq[Any] ) = FINER.doIf( l => inner.entering( sourceClass, sourceMethod, params ) );
  def exiting(sourceClass : =>String, sourceMethod : =>String) = FINER.doIf( l => inner.exiting( sourceClass, sourceMethod ) );
  def exiting(sourceClass : =>String, sourceMethod : =>String, params : =>Seq[Any] ) = FINER.doIf( l => inner.exiting( sourceClass, sourceMethod, params ) );
  def throwing(sourceClass : String, sourceMethod : String, error : Throwable) = FINER.doIf( l => inner.throwing( sourceClass, sourceMethod, error ) );
  
  
  private def levelMessage( level : MLevel, message : =>String ) = level.doIf( l => inner.log(l, message ) );
  
  def severe(message : =>String) = levelMessage(SEVERE, message);
  def warning(message : =>String) = levelMessage(WARNING, message);
  def info(message : =>String) = levelMessage(INFO, message);
  def config(message : =>String) = levelMessage(CONFIG, message);
  def fine(message : =>String) = levelMessage(FINE, message);
  def finer(message : =>String) = levelMessage(FINER, message);
  def finest(message : =>String) = levelMessage(FINEST, message);
}
