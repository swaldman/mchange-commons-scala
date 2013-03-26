package com.mchange.sc.v1.util.concurrent;

import java.util.concurrent._;

class RestrictToThreadInvoker( threadName : String ) extends Invoker {

  val latch = new CountDownLatch(1);
  var myThread : Thread = _;

  private def used = (latch.getCount == 0);

  val executorService  =  {
    val tf = new ThreadFactory {
      var used = false;
      def newThread( runnable : Runnable ) : Thread = {
	if (! used) {
	  latch.countDown();
	  myThread = {
	    val out = new Thread(runnable);
	    out.setName( threadName );
	    out.setDaemon(true);
	    out;
	  }
	  myThread
	} else {
	  throw new RuntimeException("Unsupported: Trying to re-create a Thread for a SingleThreadExecutor.");
	}
      }
    }
    val exe = Executors.newSingleThreadExecutor( tf );
    exe.submit( new Runnable(){ def run : Unit = {} } ); //prime, start the Thread immediately on construction, bind to a lazy var if you want lazy
    exe
  }

  def invokeAndWait[T]( callable : Callable[T] ) : T = {
    latch.await;
    if ( Thread.currentThread == myThread ) callable.call else executorService.submit( callable ).get();
  }

  def invokeAndWait( runnable : Runnable ) : Unit = {
    latch.await;
    if ( Thread.currentThread == myThread ) runnable.run else executorService.submit( runnable ).get();
  }

  def invokeLater( runnable : Runnable ) : Unit = {
    latch.await;
    if ( Thread.currentThread == myThread ) runnable.run else executorService.submit( runnable );
  }
}
