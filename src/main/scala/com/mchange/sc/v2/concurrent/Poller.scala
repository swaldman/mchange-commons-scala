package com.mchange.sc.v2.concurrent

import java.util.concurrent.{ScheduledThreadPoolExecutor,ScheduledFuture,TimeUnit}

import scala.concurrent.{Future,Promise}
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

object Poller {
  final class TimeoutException( label : String, deadline : Long ) extends Exception( s"Poller.Task '${label}' expired at ${new java.util.Date(deadline)}" )

  /*
   *  we separate task from Task.withDeadline so that user tasks are immutable and can be reusable
   */ 
  object Task {
    private [Poller] final case class withDeadline[T]( task : Task[T], deadline : Long )
  }
  class Task[T]( val label : String, val period : Duration, val pollFor : () => Boolean, val onSuccess : () => T, val timeout : Duration = Duration.Inf )
}
class Poller( stpe : ScheduledThreadPoolExecutor ) {

  def addTask[T]( task : Poller.Task[T] ) : Future[T] = {
    val deadline = if ( task.timeout == Duration.Inf ) -1 else System.currentTimeMillis + task.timeout.toMillis

    val promise = Promise[T]()
    scheduleTask( Poller.Task.withDeadline( task, deadline ), promise )
    promise.future
  }

  private def scheduleTask[T]( twd : Poller.Task.withDeadline[T], promise : Promise[T] ) : Unit = {

    val task     = twd.task
    val deadline = twd.deadline

    val runnable = new Runnable {

      private def timedOut = deadline >= 0 && System.currentTimeMillis > deadline

      def run() : Unit = {
        try {
          if ( ! timedOut ) {
            if ( task.pollFor() ) {
              promise.complete( Try( task.onSuccess() ) )
            } else {
              Poller.this.scheduleTask( twd, promise )
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
    stpe.schedule( runnable, millis, TimeUnit.MILLISECONDS )
  }
}
