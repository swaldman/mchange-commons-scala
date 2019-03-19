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

package com.mchange.sc.v2

import scala.util.{Try,Success,Failure}

import scala.concurrent.{Await, ExecutionContext,Future,Promise}
import scala.concurrent.duration.Duration

import scala.collection.Iterable
import scala.collection.generic.CanBuildFrom

import java.util.concurrent.{ScheduledExecutorService, ScheduledThreadPoolExecutor, ThreadFactory}

package object concurrent {

  type CBF[T] = CanBuildFrom[Seq[Future[Try[T]]], Try[T], Seq[Try[T]]]

  // because the semantics of Future sequence are poorly
  // documented, and apparently (oddly to me), the Seq
  // completes on the first failure
  //
  // see http://stackoverflow.com/questions/29344430/scala-waiting-for-sequence-of-futures
  def awaitSeq[T]( seq : Seq[Future[T]],  timeout : Duration = Duration.Inf )( implicit cbf : CBF[T], ec : ExecutionContext ) : Unit = {
    Await.ready( Future.sequence( seq.map( liftToTry ) )( cbf, ec ), timeout ) 
  }

  // see      http://stackoverflow.com/questions/29344430/scala-waiting-for-sequence-of-futures
  // see also http://stackoverflow.com/questions/15775824/how-to-carry-on-executing-future-sequence-despite-failure
  def liftToTry[T]( fut : Future[T] )( implicit ec : ExecutionContext ) : Future[Try[T]] = {
    val promise = Promise[Try[T]]()
    fut.onComplete( attempt => promise.success( attempt ) )( ec )
    promise.future
  }

  def awaitAndGatherFailures[T]( seq : Seq[Future[T]],  timeout : Duration = Duration.Inf )( implicit cbf : CBF[T], ec : ExecutionContext ) : Seq[Throwable] = {
    awaitSeq( seq, timeout )( cbf, ec )
    seq.map( _.value ).collect { case Some( Failure( ick ) ) => ick }
  }

  def awaitAndGatherLabeledFailures[L,T]( pairs : Iterable[(L,Future[T])], timeout : Duration = Duration.Inf )( implicit cbf : CBF[T], ec : ExecutionContext ) : Seq[(L,Throwable)] = {
    val pairSeq = pairs.toSeq
    awaitSeq( pairSeq.map( _._2 ), timeout )( cbf, ec )
    pairSeq.map( pair => ( pair._1, pair._2.value ) ).collect { case (label, Some( Failure( ick ) ) ) => (label, ick) }
  }

  def awaitAndGatherIndexedFailures[L,T]( seq : Seq[Future[T]], timeout : Duration = Duration.Inf )( implicit cbf : CBF[T], ec : ExecutionContext ) : Seq[(Int,Throwable)] = {
    awaitAndGatherLabeledFailures( (0 until seq.length).zip(seq) )( cbf, ec )
  }

  private [concurrent] lazy val DefaultScheduledThreadPoolExecutor : ScheduledThreadPoolExecutor = {
    val threadFactory = new ThreadFactory {
      override def newThread( r : Runnable ) : Thread = {
        val out = new Thread(r)
        out.setDaemon( true )
        out.setName("com.mchange.sc.v2.concurrent.{Poller,Scheduler}-default")
        out
      }
    }
    val ses = new ScheduledThreadPoolExecutor( scala.math.round( 1.5f * Runtime.getRuntime().availableProcessors() ) )
    ses.setThreadFactory( threadFactory )
    ses
  }
}
