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

import scala.concurrent.ExecutionContext
import java.util.concurrent.{ArrayBlockingQueue,Executors,ExecutorService,ThreadPoolExecutor,TimeUnit}
import com.mchange.sc.v2.lang.borrow
import com.mchange.v3.concurrent.BoundedExecutorService

object ExecutionContexts {
  def withExecutionContext[R]( executorService : =>ExecutorService )( op : ExecutionContext => R ) : R = {
    borrow( ExecutionContext.fromExecutorService( executorService ) )( _.shutdown )( op( _ ) )
  }
  def withFixedThreadPool[R]( size : Int )( op : ExecutionContext => R ) : R = {
    withExecutionContext( Executors.newFixedThreadPool( size ) )( op )
  }
  def withBoundedFixedThreadPool[R]( size : Int, blockBound : Int, restartBeneath : Int )( op : ExecutionContext => R ) : R = {
    require( blockBound >= size, s"blockBound must be at least the Thread pool size [size: ${size}, bound: ${blockBound}]" ) 
    withExecutionContext( new BoundedExecutorService( Executors.newFixedThreadPool( size ), blockBound, restartBeneath ) )( op )
  }
  def withCallerRunsThrottledFixedThreadPool[R]( size : Int, queueCapacity : Int )( op : ExecutionContext => R ) : R = {
    def createInner = {
      val out = new ThreadPoolExecutor( size, size, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue( queueCapacity ) )
      out.setRejectedExecutionHandler( new ThreadPoolExecutor.CallerRunsPolicy() )
      out
    }
    withExecutionContext( createInner )( op )
  }
}
