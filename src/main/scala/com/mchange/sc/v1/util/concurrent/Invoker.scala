package com.mchange.sc.v1.util.concurrent;

import java.util.concurrent.Callable;

trait Invoker {
  def invokeAndWait[T]( callable : Callable[T] ) : T;
  def invokeAndWait( runnable : Runnable ) : Unit;
  def invokeLater( runnable : Runnable ) : Unit;

  def await[T]( operation : =>T) : T = {
    val callable = new Callable[T] {
      def call : T = operation;
    }
    invokeAndWait[T]( callable );
  }

  def later( operation : =>Unit) : Unit = {
    val runnable = new Runnable {
      def run : Unit = operation;
    }
    invokeLater( runnable );
  }
}
