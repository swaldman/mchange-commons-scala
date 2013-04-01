package com.mchange.sc.v1.util.concurrent;

import org.specs2.Specification;

object RestrictToThreadInvokerSpec {
  lazy val invoker = new RestrictToThreadInvoker( getClass.getName );
}

class RestrictToThreadInvokerSpec extends Specification {
  import RestrictToThreadInvokerSpec._;

  def is = {
    "A RestrictToThreadInvoker"                                                                    ^
    "can run and await return of a task"                                        ! runATask         ^
    "can survive an Exception                        "                          ! surviveException ;
  }

  def runATask = invoker.await( math.sqrt( 100 ) ) == 10;
  def surviveException = {
    try {
      invoker.await( throw new RuntimeException );
    } catch {
      case e : Exception => println("swallowed an Exception."); //swallow
    }
    runATask
  }
}
