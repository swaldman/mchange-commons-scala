/*
package com.mchange.sc.v1.concurrent;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

class PimpedExecutorService( val executor : ExecutorService ) {
  def invokeAndWait[T]( callable : Callable[T] ) : T = executor.submit( callable ).get();
  def invokeAndWait( runnable : Runnable ) : Unit = executor.submit( runnable ).get();
  def invokeLater( runnable : Runnable ) : Unit = executor.submit( runnable );
}
*/
