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

import java.util.concurrent.{ScheduledExecutorService,ScheduledThreadPoolExecutor,ScheduledFuture,TimeUnit}

import scala.concurrent.{Future,Promise}
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal


object ScheduledExecutorServicePoller {
  abstract class Abstract( protected val ses : ScheduledExecutorService ) extends Poller {
    // MT: protected by this' lock
    var closed = false

    def isClosed : Boolean = this.synchronized { closed }

    def addTask[T]( task : Poller.Task[T] ) : Future[T] = {

      val promise = Promise[T]()
      scheduleTask( Poller.Task.withDeadline( task ), promise, true )
      promise.future
    }

    private def scheduleTask[T]( twd : Poller.Task.withDeadline[T], promise : Promise[T], first : Boolean ) : Unit = {
      if ( isClosed ) {
        promise.failure( new Poller.ClosedException( this ) )
      } else {
        val task     = twd.task
        val deadline = twd.deadline

        val runnable = new Runnable {

          def run() : Unit = {
            try {
              if ( ! twd.timedOut ) {
                task.pollFor() match {
                  case Some( value ) => promise.success( value )
                  case None          => Abstract.this.scheduleTask( twd, promise, false )
                }
              } else {
                promise.failure( new Poller.TimeoutException( task.label, deadline ) )
              }
            }
            catch {
              case NonFatal( unexpected ) => promise.failure( unexpected )
            }
          }
        }

        if ( first && task.pollImmediately ) {
          ses.submit( runnable ) // initial check should be immediate
        }
        else {
          val millis = task.period.toMillis
          ses.schedule( runnable, millis, TimeUnit.MILLISECONDS )
        }
      }
    }

    /**
      * Since this implementation accepts an externally constructed ScheduledExecutorService, 
      * we don't shut it down, just prevent new tasks from getting scheduled.
      */ 
    def close() : Unit = this.synchronized {
      closed = true
    }
  }
  final class withExternalExecutor( ses : ScheduledExecutorService ) extends ScheduledExecutorServicePoller.Abstract( ses )

  final class withInternalExecutor( corePoolSize : Int = 3 ) extends ScheduledExecutorServicePoller.Abstract( new ScheduledThreadPoolExecutor( corePoolSize ) ) {
    /**
      * Here we construct and cleanup our own ScheduledThreadPoolExecutor
      */ 
    override def close() : Unit = {
      super.close()
      ses.shutdown() // gracefully shutdown the threadpool after existing tasks have executed
    }
  }
}

