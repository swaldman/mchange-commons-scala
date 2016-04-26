package com.mchange.sc.v2.concurrent;

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.Try

/**
  * Rather than waiting on a collection of futures,
  * which might use up a lot of heap if the collection is very large,
  * this class defines a "Manager" one can wait on for a collection
  * of futures to complete.
  */ 
object ManagedFuture {
  final class RegistrationStateException( message : String ) extends Exception( message )

  object Manager {
    import scala.util.Random
    import scala.math.abs

    def apply( name : String ) : Manager = new Manager( name )
    def create( name : String = s"ManagedFuture.Manager-${abs(Random.nextLong)}" ) = apply( name )
  }
  class Manager( val name : String ) {

    // MT: protected by this' lock
    private[this] var count = 0L
    private[this] var failureList : List[Throwable] = Nil
    private[this] var registrationsClosed = false

    /**
      * Don't forget to call closeRegistrations(), or this
      * will await forever!
      */ 
    def await() : Unit = this.synchronized {
      while (!allDone) this.wait()
    }
    def allDone : Boolean = this.synchronized {
      registrationsClosed && count == 0
    }
    def remaining : Long = this.synchronized {
      count
    }
    def failures = this.synchronized {
      failureList
    }
    def registrationsAreClosed = this.synchronized {
      registrationsClosed
    }
    def closeRegistrations() : Unit = this.synchronized {
      registrationsClosed = true
    }

    private[ManagedFuture] def register() = this.synchronized {
      if ( registrationsClosed )
        throw new RegistrationStateException(s"Registrations have been closed on ${this}")
      count += 1
    }

    /**
      *  called in the rare case that a future cannot be instantiated following registration
      *  on that future's behalf
      */
    private[ManagedFuture] def withdrawRegistration() = this.synchronized {
      count -= 1
      checkNotify()
    }
    private[ManagedFuture] def markSuccess() : Unit = this.synchronized {
      count -= 1
      checkNotify()
    }
    private[ManagedFuture] def markFailure( failure : Throwable ) : Unit = this.synchronized {
      failureList = failure :: failureList
      count -= 1
      checkNotify()
    }

    // call with this' lock
    private[this] def checkNotify() : Unit = if ( allDone ) this.notifyAll()

    override def toString() = s"ManagedFuture.Manager(${name})"
  }

  def apply[T]( manager : Manager )( body : =>T )( implicit executor : ExecutionContext ) : ManagedFuture[T] = new Uncertain( manager, body )( executor )

  /**
    * This is "managed" in the sense that it will throw an Exception to the caller
    * if registrations have been closed on the manager.
    */
  def successful[T]( manager : Manager )( goodT : T ) : ManagedFuture[T] = new Presucceeded[T]( manager, goodT )

  /**
    * This is "managed" in the sense that it will throw an Exception to the caller
    * if registrations have been closed on the manager, and the failures
    * will be recorded among the manager's failures.
    */
  def failed[T]( manager : Manager )( failure : Throwable ) : ManagedFuture[T] = new Prefailed[T]( manager, failure )

  trait Base[T] extends ManagedFuture[T] {
    val inner : Future[T]

    def isCompleted : Boolean = inner.isCompleted

    def onComplete[U](f : (Try[T]) => U)(implicit executor: ExecutionContext) : Unit = inner.onComplete(f)(executor)

    def ready( atMost : Duration )( implicit permit : CanAwait ) : Base.this.type = {
      inner.ready( atMost )( permit )
      this
    }

    def result( atMost : Duration )( implicit permit : CanAwait ) : T = {
      inner.result( atMost )( permit )
    }

    def value : Option[Try[T]] = inner.value
  }

  class Uncertain[T] private[ManagedFuture] ( manager : ManagedFuture.Manager, body : => T )( implicit ec : ExecutionContext ) extends Base[T] {
    val inner = {
      // we've gotto register synchronously, so that clients can guarantee
      // regsitrations happen before manager.closeRegistration()
      manager.register() 
      try {
        Future {
          try {
            val out = body
            manager.markSuccess()
            out
          } catch {
            case t : Throwable => {
              manager.markFailure(t)
              throw t
            }
          }
        }
      } catch {
        case t : Throwable => {
          manager.withdrawRegistration()
          throw t
        }
      }
    }
  }
  class Presucceeded[T] private[ManagedFuture] ( manager : Manager, goodT : T ) extends Base[T] {
    // seems like overkill, but we need to throw the Exception if registrations are closed
    val inner = {
      val out = Future.successful[T]( goodT )
      manager.register() 
      manager.markSuccess()
      out
    }
  }
  class Prefailed[T] private[ManagedFuture] ( manager : Manager, failure : Throwable ) extends Base[T] {
    // seems like overkill, but we need to throw the Exception if registrations are closed
    val inner = {
      val out = Future.failed[T]( failure )
      manager.register() 
      manager.markFailure( failure )
      out
    }
  }
}
sealed trait ManagedFuture[T] extends Future[T]
