package com.mchange.sc.v1.reconcile;

class CantReconcileException(message : String, cause : Throwable) extends Exception( message, cause )
{
  def this( message : String ) = this( message, null );
  def this( cause : Throwable ) = this( null, cause );
  def this() = this( null, null );
}

object CantReconcileException
{
    def ouch(irreconcilable1 : Any, irreconcilable2 : Any) : Nothing =
    { throw new CantReconcileException( "Both fields have distinct values, cannot reconcile -- %s and %s.".format(irreconcilable1, irreconcilable2) ); }
}

