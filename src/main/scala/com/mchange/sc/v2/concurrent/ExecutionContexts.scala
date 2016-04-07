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
