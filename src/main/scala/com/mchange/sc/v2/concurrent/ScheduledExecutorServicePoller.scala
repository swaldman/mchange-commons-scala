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
      scheduleTask( Poller.Task.withDeadline( task ), promise )
      promise.future
    }

    private def scheduleTask[T]( twd : Poller.Task.withDeadline[T], promise : Promise[T] ) : Unit = {
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
                  case None          => Abstract.this.scheduleTask( twd, promise )
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

        val millis = task.period.toMillis
        ses.schedule( runnable, millis, TimeUnit.MILLISECONDS )
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

