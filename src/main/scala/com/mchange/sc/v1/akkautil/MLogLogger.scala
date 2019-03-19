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

package com.mchange.sc.v1.akkautil;

import akka.actor._;
import akka.event.Logging._;
import akka.event.DummyClassForStringSources;

import com.mchange.sc.v1.log._;

object MLogLogger {
  implicit lazy val log = MLogger( this );
}

// adapted from akka.event.slf4j.Slf4jLogger
class MLogLogger extends Actor {

  private def logFormat( logger : MLogger, level : MLevel, message : Any ) : Unit = {
    stringFormat( message ) match {
      case string : String => logger.log( level, string );
      case array : Array[_] => logger.logFormat( level, "{}", array ); // note this would be one-element array made by stringFormat
      case other =>  {
	throw new InternalError( s"Unexpected result from StringMessage, orig message: '${message}', transformed: '${other}'" );
      }
    }
  }

  /* returns String or Array only */ 
  private def stringFormat( message : Any ) : AnyRef = message match {
    case string : String => string;
    case array : Array[_] => array.mkString("[",", ","]");
    case seq : Seq[_] => seq.mkString("[",", ","]");
    case null => "null";
    case other => Array(message);
  }

  private def logger( logClass : Class[_],  logSource : String ) : MLogger = {
    if ( logClass == classOf[DummyClassForStringSources] )
      MLogger( logSource );
    else
      MLogger( logClass );
  }

  def receive = {

    case event @ Error(cause, logSource, logClass, message) => cause match {
      case Error.NoCause | null => {
	if ( message == null )
	  logger(logClass, logSource).log(MLevel.SEVERE, event.toString + " <Unknown Cause>")
	else
	  logFormat( logger(logClass, logSource), MLevel.SEVERE, message );
      }
      case cause => {
	if (message == null)
	  logger(logClass, logSource).log(MLevel.SEVERE, cause.getLocalizedMessage(), cause);
	else {
	  stringFormat( message ) match {
	    case string : String => logger(logClass, logSource).log(MLevel.SEVERE, string, cause);
	    case array : Array[_] => logger(logClass, logSource).log( MLevel.SEVERE, java.text.MessageFormat.format("{}", array ), cause );
	    case other => {
	      throw new InternalError( s"Unexpected result from StringMessage, orig message: '${message}', transformed: '${other}'" );
	    }
	  }
	}
      }
    }
    case event @ Warning(logSource, logClass, message) => logFormat( logger(logClass, logSource), MLevel.WARNING, message );
    case event @ Info(logSource, logClass, message) => logFormat( logger(logClass, logSource), MLevel.INFO, message );
    case event @ Debug(logSource, logClass, message) => logFormat( logger(logClass, logSource), MLevel.FINER, message );

    case InitializeLogger(_) ⇒
      MLogLogger.log.info("MLogLogger started")
      sender ! LoggerInitialized
  }
}
