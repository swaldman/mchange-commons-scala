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

package com.mchange.sc.v2;

import com.mchange.sc.v2.util.EitherAsMonad;

import com.mchange.sc.v1.log.{MLogger,MLevel};
import com.mchange.sc.v1.log.MLevel._;

import scala.util.{Try, Success, Failure};

import scala.language.implicitConversions;

package object failable {
  class UnhandledFailException( fail : Fail ) extends Exception( fail.toString, fail.source match { case th : Throwable => th; case _ => null } );

  val lineSeparator = scala.util.Properties.lineSeparator;

  // kind of yuk, but we've renamed this from "Failure" to "Fail" to avoid inconvenient
  // need to qualify names when working with scala.util.Failure.
  final object Fail {
    def simple( message : String ) : Fail = Fail( message, message, None );
    val EmptyFailable : Fail = Fail("An attempt to filter or pattern-match a Failable failed, leaving EmptyFailable.", "EmptyFailable", None);
  }
  final case class Fail( message : String, source : Any, mbStackTrace : Option[Array[StackTraceElement]] ) {

    override def toString() : String = mbStackTrace.fold( message ) { stackTrace =>
      (List( message ) ++ stackTrace).mkString( lineSeparator )
    }
    def vomit : Nothing = throw new UnhandledFailException( this );
  }

  trait FailSource[T] {
    def getMessage( source : T ) : String;
    def getStackTrace( source : T ) : Array[StackTraceElement] = Thread.currentThread().getStackTrace();

    def getFail( source : T, includeStackTrace : Boolean = true ) : Fail = {
      val mbStackTrace = if ( includeStackTrace ) Some( getStackTrace( source ) ) else None;
      Fail( getMessage( source ), source, mbStackTrace )
    }
  }

  implicit final object StringAsFailSource extends FailSource[String] {
    def getMessage( source : String ) : String = source;
  }

  implicit final object ThrowableAsFailSource extends FailSource[Throwable] {
    def getMessage( source : Throwable ) : String = s"${source.getClass.getName}: ${source.getMessage()}";

    override def getStackTrace( source : Throwable ) = source.getStackTrace;
  }

  type Failable[+T] = Either[Fail,T];

  // right-bias Failable[T], for convenience and to render its API more analogous to Option[T]
  private val FailableAsMonad = EitherAsMonad.RightBiased.WithEmptyToken[Fail]( Fail.EmptyFailable );
  implicit final class FailableOps[T]( failable : Failable[T] ) extends EitherAsMonad.RightBiased.WithEmptyToken.AbstractOps( failable )( FailableAsMonad ) {
    override def get : T = failable match {
      case Left( fail )   => fail.vomit;
      case Right( value ) => value;
    }
    def fail : Fail = failable.left.get;
    //other methods
    def flatten[U](implicit evidence : T <:< Failable[U]) : Failable[U] = {
      failable match {
        case oops @ Left( _ )  => refail( oops );
        case        Right( t ) => evidence( t );
      }
    }
    def recover[TT >: T]( f : Fail => TT ) : Failable[TT] = {
      failable match {
        case      Left( fail ) => succeed( f( fail ) )
        case ok @ Right( _ )   => ok;
      }
    }
    def recover[TT >: T]( recoveryValue : TT ) : Failable[TT] = recover( _ => recoveryValue )

    def isSuccess : Boolean = failable.isRight;
    def isFail    : Boolean = !isSuccess;
    def isFailure : Boolean = isFail;

    def toWarnable( recoveryFunction : Fail => T ) : Warnable[T] = {
      def recoveryWarnable( oops : Fail ) = {
        val recoveryValue = recoveryFunction( oops );
        Warnable[T]( List( Fail.simple(s"Using recovery value: ${recoveryValue}"), oops ), recoveryValue );
      }
      this.fold( recoveryWarnable _ )( t => Warnable[T]( Nil, t ) )
    }
    def toWarnable( recovery : T ) : Warnable[T] = {
      def recoveryWarnable( oops : Fail ) = Warnable[T]( List( Fail.simple(s"Using recovery default: ${recovery}"), oops ), recovery );
      this.fold( recoveryWarnable _ )( t => Warnable[T]( Nil, t ) )
    }
  }

  def fail[S : FailSource]( source : S, includeStackTrace : Boolean = true ) : Failable[Nothing] = {
    val ms = implicitly[FailSource[S]];
    val failure = ms.getFail( source, includeStackTrace );
    Left( failure ) : Failable[Nothing];
  }

  /**
   * A utility to re-establish the irrelevant right type as universally acceptable Nothing
   */  
  def refail( prefail : Left[Fail,Any] ) : Failable[Nothing] = prefail.asInstanceOf[Failable[Nothing]]

  def succeed[T]( value : T ) : Failable[T] = Right( value );

  val Poop : PartialFunction[Throwable, Failable[Nothing]] = { case scala.util.control.NonFatal( t : Throwable ) => fail( t ) }

  implicit class FailableTry[T]( val attempt : Try[T] ) extends AnyVal {
    def toFailable : Failable[T] = attempt match {
      case Success( value )     => succeed( value );
      case Failure( exception ) => fail( exception, true );
    }
  }

  implicit class FailableOption[T]( val maybe : Option[T] ) extends AnyVal {
    def toFailable[ U : FailSource ]( source : U = "No information available." ) : Failable[T] = {
      maybe match {
        case Some( value )  => succeed( value );
        case None           => fail( source, true );
      }
    }
  }

  implicit class FailableLoggingOps[T]( val failable : Failable[T] ) extends AnyVal {
    def log( level : MLevel, premessage : => String  = "" )( implicit logger : MLogger ) : Failable[T] = {
      def doLog( oops : Fail ) = {
        val pm = premessage; // avoid multiple executions of the by name expression
        val prefix = if ( pm == "" || pm == null ) "" else pm + lineSeparator;
        level.log( prefix + oops )( logger )
      }
      failable match {
        case Left( oops ) => doLog( oops );
        case Right( _ )   => /* ignore */;
      }
      failable
    }
    def logRecover[TT >: T]( level : MLevel, recoveryFunction : Fail => TT, premessage : => String )( implicit logger : MLogger ) : Failable[TT] = {
      log( level, premessage )( logger ).recover( recoveryFunction );
    }
    def logRecover[TT >: T]( level : MLevel, recoveryFunction : Fail => TT )( implicit logger : MLogger ) : Failable[TT] = logRecover[TT]( level, recoveryFunction, "" )( logger );

    def logRecover[TT >: T]( level : MLevel, recoveryValue : TT, premessage : => String )( implicit logger : MLogger ) : Failable[TT] = {
      log( level, premessage )( logger ).recover( recoveryValue );
    }
    def logRecover[TT >: T]( level : MLevel, recoveryValue : TT )( implicit logger : MLogger ) : Failable[TT] = logRecover( level, recoveryValue, "" )( logger );

    // is the API below just a little too cute?
    def warning( premessage : => String = "" )( implicit logger : MLogger ) : Failable[T] = log( WARNING, premessage )( logger )
    def severe( premessage : => String = "" )( implicit logger : MLogger )  : Failable[T] = log( SEVERE, premessage )( logger )
    def info( premessage : => String = "" )( implicit logger : MLogger )    : Failable[T] = log( INFO, premessage )( logger )
    def debug( premessage : => String = "" )( implicit logger : MLogger )   : Failable[T] = log( DEBUG, premessage )( logger )
    def trace( premessage : => String = "" )( implicit logger : MLogger )   : Failable[T] = log( TRACE, premessage )( logger )

    def warn( premessage : => String = "" )( implicit logger : MLogger ) : Failable[T] = warning( premessage )( logger )
  }

  case class Warnable[+T]( warnings : List[Fail], result : T ) {
    def map[Y]( f : T => Y )               : Warnable[Y] = Warnable[Y]( this.warnings, f( result ) );
    def flatMap[Y]( f : T => Warnable[Y] ) : Warnable[Y] = map( f ).flatten;
    def clearWarnings                      : Warnable[T] = this.copy( warnings=Nil );
  }
  implicit class NestingWarnableOps[Y]( val nesting : Warnable[Warnable[Y]] ) extends AnyVal {
    def flatten : Warnable[Y] = Warnable[Y]( nesting.result.warnings ::: nesting.warnings, nesting.result.result );
  }

  implicit class WarnableLoggingOps[T]( val warnable : Warnable[T] ) extends AnyVal {
    private def logWarnings( level : MLevel, premessage : =>String, tag : =>String, clear : Boolean )( implicit logger : MLogger ) : Warnable[T] = {
      if (! warnable.warnings.isEmpty ) { // avoid computation of by-name premessage if there is nothing to warn
        val pm = premessage;              // avoid multiple computations of by-name premessage
        val t = tag;
        val linePrefix = if ( t == null ) "Execution Warning -> " else t;
        if (pm != null && pm != "") level.log( pm );
        warnable.warnings.foreach( oops => level.log( linePrefix + oops )( logger ) );
        if (clear) warnable.clearWarnings else warnable
      } else {
        warnable
      }
    }
    def log( level : MLevel = WARNING, premessage : =>String = null, tag : =>String = null )( implicit logger : MLogger ) = logWarnings( level, premessage, tag, false )( logger );
    def logClear( level : MLevel = WARNING, premessage : =>String = null, tag : =>String = null )( implicit logger : MLogger ) = logWarnings( level, premessage, tag, true )( logger );

    def debugLog( premessage : =>String = null, tag : =>String = null )( implicit logger : MLogger ) = log( DEBUG, premessage, tag )( logger );
    def debugLogClear( premessage : =>String = null, tag : =>String = null )( implicit logger : MLogger ) = logClear( DEBUG, premessage, tag )( logger );
  }
}

