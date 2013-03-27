package com.mchange.sc.v1;

import scala.util.{Either,Left,Right};

package object util {

  val NullFunction = new Function0[Unit] with Runnable { 
    def apply : Unit = {}
    def run : Unit = apply;
  }

  def failOrGo[S] ( operation : =>S ) : Either[Throwable,S] = {
    try { Right( operation ) }
    catch {
      case e : Exception => Left(e);
    }
  }
}
