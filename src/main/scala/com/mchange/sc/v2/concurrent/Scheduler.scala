package com.mchange.sc.v2.concurrent

import scala.concurrent.{ExecutionContext,Future,Promise}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal
import scala.util.Failure
import java.util.concurrent.{ScheduledExecutorService,ScheduledFuture,ScheduledThreadPoolExecutor,TimeUnit}
import com.mchange.sc.v2

import com.mchange.sc.v1.log.MLevel._

object Scheduler {

  private implicit lazy val logger = mlogger( this )

  type Task[T] = () => T

  class SchedulerException( message : String, cause : Throwable = null ) extends Exception( message, cause )
  final class CancelledException extends SchedulerException(null, null)
  final class ClosedException( instance : Scheduler ) extends SchedulerException( s"Secheduler '${instance}' has been closed." )

  trait Scheduled[T] {
    def delayUntilNext  : Option[Duration]
    def future          : Future[T]
    def attemptCancel() : Unit

    def isCanceled() : Boolean = {
      future.value match {
        case Some( Failure( _ : CancelledException ) ) => true
        case _                                         => false
      }
    }
  }
  final object ScheduledExecutorService {
    private val DefaultTimeUnit = TimeUnit.MILLISECONDS


    private final object ConsistentTimeUnits {
      def apply( initialDelay : Duration, period : Duration ) : ConsistentTimeUnits = {
        if ( initialDelay.unit == period.unit ) {
          ConsistentTimeUnits( initialDelay.length, period.length, period.unit )
        }
        else {
          // we choose nanos as the consistent unit only to minimize rounding error
          val initialDelayNanos = initialDelay.toNanos
          val periodNanos       = period.toNanos
          ConsistentTimeUnits( initialDelayNanos, periodNanos, TimeUnit.NANOSECONDS )
        }
      }
    }
    private final case class ConsistentTimeUnits( initialDelay : Long, period : Long, unit : TimeUnit )

    private def bestAttemptCancel( sf : ScheduledFuture[_] ) : Throwable = { // null unless the Exception is fatal
      try {
        sf.cancel( false )
        null
      }
      catch {
        case NonFatal(t)   => {
          DEBUG.log("Exception while trying to cancel ScheduledFuture", t)
          null
        }
        case t : Throwable => {
          SEVERE.log("FATAL exception while trying to cancel ScheduledFuture", t)
          t
        }
      }
    }
    private def bestAttemptFail( promise : Promise[_], t : Throwable ) : Throwable = { // null unless the Exception is fatal
      try {
        promise.failure( t )
        null
      }
      catch {
        case NonFatal(t)   => {
          DEBUG.log("Exception while trying to fail a promis", t)
          null
        }
        case t : Throwable => {
          SEVERE.log("FATAL exception while trying to fail a promise", t)
          t
        }
      }
    }
    private def bestAttemptFailCancel( promise : Promise[_], t : Throwable, sf : ScheduledFuture[_] ) : Unit = {
      val fail = bestAttemptFail( promise, t )
      val cancel = bestAttemptCancel( sf )
      if ( fail != null ) throw fail else if ( cancel != null ) throw cancel
    }

    private class RepeatingScheduled[T]( doSchedule : Runnable => ScheduledFuture[_], task : Scheduler.Task[T] ) extends Scheduler.Scheduled[Unit] {
      private val promise = Promise[Unit]()

      val runnable = new Runnable {
        override def run() : Unit = {
          try {
            task()
          }
          catch {
            case t : Throwable => bestAttemptFailCancel( promise, t, sf )
          }
        }
      }

      val sf : ScheduledFuture[_] = doSchedule( runnable )

      def delayUntilNext : Option[Duration] = {
        val rawDelay = sf.getDelay(DefaultTimeUnit)
        if ( rawDelay < 0 ) {
          None
        }
        else {
          Some( Duration( rawDelay, DefaultTimeUnit ) )
        }
      }
      val future : Future[Unit] = promise.future
      def attemptCancel() : Unit = bestAttemptFailCancel( promise, new CancelledException, sf )
    }
    private class OneTimeScheduled[T]( doSchedule : Runnable => ScheduledFuture[_], task : Scheduler.Task[T], executionContext : ExecutionContext ) extends Scheduler.Scheduled[T] {

      private val promise = Promise[Future[T]]()

      class Precancelable extends Runnable {

        //MT: synchronized on this' lock
        private var canceled = false
        private var started  = false

        override def run() : Unit = {
          this.synchronized {
            if (! canceled ) {
              this.started = true

              try {
                promise.success( Future( task() )( executionContext ) )
              }
              catch {
                case t : Throwable => bestAttemptFailCancel( promise, t, sf )
              }
            }
          }
        }

        def attemptCancel() : Boolean = this.synchronized {
          if ( started ) {
            false
          }
          else {
            canceled = true
            bestAttemptFailCancel( promise, new CancelledException, sf )
            true
          }
        }
      }

      private val pc = new Precancelable()

      private val sf = doSchedule( pc )

      def attemptCancel() : Unit      = pc.attemptCancel
      val future          : Future[T] = promise.future.flatMap( identity )( executionContext ) // promise.future.flatten -- unfortunately, no flatten method pre-2.12
      def delayUntilNext  : Option[Duration] = {
        val rawDelay = sf.getDelay(DefaultTimeUnit)
        if ( rawDelay < 0 ) {
          None
        }
        else {
          Some( Duration( rawDelay, DefaultTimeUnit ) )
        }
      }
    }
    abstract class Abstract( protected val ses : ScheduledExecutorService ) extends Scheduler {
      // MT: Protected by this' lock
      var closed = false

      def isClosed = this.synchronized { closed }

      def assertNotClosed() : Unit = this.synchronized {
        if ( closed ) throw new ClosedException( this )
      }

      def close() : Unit = this.synchronized {
        closed = true
      }

      private def reportError( t : Throwable ) : Unit = WARNING.log( s"An error occurred within ExecutionContext ${executionContext}", t )

      private val executionContext = ExecutionContext.fromExecutorService( ses, reportError _ )

      def schedule[T]( task : Scheduler.Task[T], delay : Duration ) : Scheduler.Scheduled[T] = {
        assertNotClosed()
        val doSchedule : Runnable => ScheduledFuture[_] = runnable => ses.schedule( runnable, delay.length, delay.unit )
        new OneTimeScheduled[T]( doSchedule, task, executionContext )
      }
      def scheduleAtFixedRate( task : Scheduler.Task[Any], initialDelay : Duration, period : Duration ) : Scheduler.Scheduled[Unit] = {
        assertNotClosed()
        val ctu = ConsistentTimeUnits( initialDelay, period )
        val doSchedule : Runnable => ScheduledFuture[_] = runnable => ses.scheduleAtFixedRate( runnable, ctu.initialDelay, ctu.period, ctu.unit )
        new RepeatingScheduled( doSchedule, task )
      }
      def scheduleWithFixedDelay( task : Scheduler.Task[Any], initialDelay : Duration, delay : Duration ) : Scheduler.Scheduled[Unit] = {
        assertNotClosed()
        val ctu = ConsistentTimeUnits( initialDelay, delay )
        val doSchedule : Runnable => ScheduledFuture[_] = runnable => ses.scheduleWithFixedDelay( runnable, ctu.initialDelay, ctu.period, ctu.unit )
        new RepeatingScheduled( doSchedule, task )
      }
    }
  }
  final class withExternalExecutor( ses : ScheduledExecutorService ) extends Scheduler.ScheduledExecutorService.Abstract( ses )

  final class withInternalExecutor( corePoolSize : Int = 3 ) extends Scheduler.ScheduledExecutorService.Abstract( new ScheduledThreadPoolExecutor( corePoolSize ) ) {
    /**
      * Here we construct and cleanup our own ScheduledThreadPoolExecutor
      */ 
    override def close() : Unit = {
      super.close()
      ses.shutdown() // gracefully shutdown the threadpool after existing tasks have executed
    }
  }

  implicit lazy val Default : Scheduler = {
    class DefaultScheduler extends Scheduler.ScheduledExecutorService.Abstract( DefaultScheduledThreadPoolExecutor ) {
      override def close() : Unit = {
        throw new SchedulerException( "Scheduler.Default cannot be close()ed. Define your own Scheduler instance if you wish to manage its lifecycle." )
      }
    }
    new DefaultScheduler
  }

}
trait Scheduler extends AutoCloseable {
  def schedule[T]( task : Scheduler.Task[T], delay : Duration )                                     : Scheduler.Scheduled[T]   
  def scheduleAtFixedRate( task : Scheduler.Task[Any], initialDelay : Duration, period : Duration )   : Scheduler.Scheduled[Unit]
  def scheduleWithFixedDelay( task : Scheduler.Task[Any], initialDelay : Duration, delay : Duration ) : Scheduler.Scheduled[Unit]
  def close() : Unit
}
