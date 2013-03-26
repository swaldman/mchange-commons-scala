package com.mchange.sc.v1.util.concurrent;

import java.util.concurrent.Callable;

trait Invoker {
  def invokeAndWait[T]( callable : Callable[T] ) : T;
  def invokeAndWait( runnable : Runnable ) : Unit;
  def invokeLater( runnable : Runnable ) : Unit;
}
