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
    final case class withDeadline[T] ( task : Task[T], deadline : Long ) {
      def timedOut = deadline >= 0 && System.currentTimeMillis > deadline
    }

    def apply[T]( label : String, period : Duration, pollFor : () => Option[T], timeout : Duration = Duration.Inf ) = new Task( label, period, pollFor, timeout )
  }

  // we DON'T make Task final or a case class so that subclasses and objects with values filled in can be defined
  class Task[T]( val label : String, val period : Duration, val pollFor : () => Option[T], val timeout : Duration = Duration.Inf ) {
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
