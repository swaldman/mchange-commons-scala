package com.mchange.sc.v2;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

package object lang {

  implicit val logger = MLogger( "com.mchange.sc.v2.lang" );

  def attemptClose( rsrc : AutoCloseable, t : Throwable ) = {
    try { if (rsrc != null) rsrc.close(); }
    catch {
      case e : Exception => {
        if ( t != null )
          t.addSuppressed( e );
        FINEST.log("Suppressed Exception on close().", e);
      }
    }
  }

  def borrow[R <: AutoCloseable,A]( rsrc : =>R )( op : R => A ) : A = {
    var throwable : Throwable = null;
    try { op( rsrc ) }
    catch {
      case t : Throwable => {
        throwable = t;
        throw t;
      }
    }
    finally { attemptClose( rsrc, throwable ); }
  }

}
