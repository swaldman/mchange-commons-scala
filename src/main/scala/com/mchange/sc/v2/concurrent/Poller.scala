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

package com.mchange.sc.v2.concurrent

import scala.concurrent.Future
import scala.concurrent.duration._

import java.util.concurrent.ScheduledExecutorService


object Poller {
  class PollerException( message : String, cause : Throwable = null ) extends Exception( message, cause )
  final class TimeoutException( label : String, deadline : Long ) extends PollerException( s"Poller.Task '${label}' expired at ${new java.util.Date(deadline)}" )
  final class ClosedException( instance : Poller ) extends PollerException( s"Poller '${instance}' has been closed." )

  implicit lazy val Default = new Poller {
    val inner = new ScheduledExecutorServicePoller.withExternalExecutor( DefaultScheduledThreadPoolExecutor )

    override def addTask[T]( task : Poller.Task[T] ) : Future[T] = inner.addTask( task )

    override def close() : Unit = {
      throw new PollerException( "Poller.Default cannot be close()ed. Define your own Poller instance if you wish to manage its lifecycle." )
    }
  }

  def withExternalExecutor( ses : ScheduledExecutorService ) : Poller = new ScheduledExecutorServicePoller.withExternalExecutor( ses )

  def withInternalExecutor( corePoolSize : Int = 3 ) : Poller = new ScheduledExecutorServicePoller.withInternalExecutor( corePoolSize )

  object Task {
  /**
    *  We separate task from Task.withDeadline[T] so that user tasks are immutable and can be reusable.
    * 
    *  Task.wthDeadline[T] useful to implementations, not clients or users of a Poller.
    */ 
    object withDeadline {
      def apply[T]( task : Task[T] ) : Task.withDeadline[T] = {
        val deadline = if ( task.timeout == Duration.Inf ) -1 else System.currentTimeMillis + task.timeout.toMillis
        this.apply( task, deadline )
      }
    }
    /**
      * @param deadline, as Long in milliseconds since UNIX epoch, negative values mean no deadline!
      */ 
    final case class withDeadline[T] private ( task : Task[T], deadline : Long ) {
      def timedOut = deadline >= 0 && System.currentTimeMillis > deadline
    }

    def apply[T]( label : String, period : Duration, pollFor : () => Option[T], timeout : Duration = Duration.Inf ) = new Task( label, period, pollFor, timeout )
  }

  // we DON'T make Task final or a case class so that subclasses and objects with values filled in can be defined
  class Task[T]( val label : String, val period : Duration, val pollFor : () => Option[T], val timeout : Duration = Duration.Inf, val pollImmediately : Boolean = true ) {
    override def toString : String = s"""Poller.Task( label="${label}", period=${period}, timeout=${timeout}"""
  }
}
trait Poller extends AutoCloseable {
  def addTask[T]( task : Poller.Task[T] ) : Future[T]

  /**
    * Requests the poller stop polling (signalling failures if necessary on outstanding polls)
    * and clean up any resources it  may have opened.
    * 
    * There is no guarantee that a poll will not succeed after a Poller has been shutdown,
    * although "pretty soon" (in practice likely to mean after `task.period`), polls should begin
    * to fail.
    * 
    * Whether any underlying Threads used to implement the poller are stopped is implementation dependent.
    * Implementations that internally spawn their own Threads should shut them down. Those that make use of
    * external Thread pools or utilities should leave those external utilities active until their creators
    * shut them down.
    */ 
  def close() : Unit
}
