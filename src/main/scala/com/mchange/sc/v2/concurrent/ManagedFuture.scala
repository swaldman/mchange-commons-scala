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

  def apply[T]( manager : Manager )( body : =>T )( implicit executor : ExecutionContext ) = new ManagedFuture( manager, body )( executor )
}
class ManagedFuture[T] private ( manager : ManagedFuture.Manager, body : => T )( implicit ec : ExecutionContext ) extends Future[T] {
  val inner = Future {
    try {
      manager.register()
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

  def isCompleted : Boolean = inner.isCompleted

  def onComplete[U](f : (Try[T]) => U)(implicit executor: ExecutionContext) : Unit = inner.onComplete(f)(executor)

  def ready( atMost : Duration )( implicit permit : CanAwait ) : ManagedFuture.this.type = {
    inner.ready( atMost )( permit )
    this
  }

  def result( atMost : Duration )( implicit permit : CanAwait ) : T = {
    inner.result( atMost )( permit )
  }

  def value : Option[Try[T]] = inner.value
}
