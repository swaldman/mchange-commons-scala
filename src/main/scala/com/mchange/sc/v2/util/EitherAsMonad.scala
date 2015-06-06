package com.mchange.sc.v2.util;

import scala.util.{Either,Left,Right}

object EitherAsMonad {

  /**
    * empty may be a Nothing, that is the throwing of an Exception
    * if the intention is to assert that no non-matching patterns 
    * are to be permitted
    */ 
  trait WithEmpty[+T] {
    def empty : T;
  }
  abstract class RightBiased[X,Y]( val empty : X ) extends WithEmpty[X]{
    val leftEmpty = Left(empty);

    def flatMap[XX >: X, Z]( src : Either[X,Y] )( f : Y => Either[XX,Z] ) : Either[XX,Z] = {
      src match {
        case Left( _ )  => src.asInstanceOf[Left[X,Z]]
        case Right( y ) => f( y )
      }
    }
    def map[Z]( src : Either[X,Y] )( f : Y => Z ) : Either[X,Z] = {
      src match {
        case Left( _ )  => src.asInstanceOf[Left[X,Z]]
        case Right( y ) => Right( f( y ) )
      }
    }
    def withFilter( src : Either[X,Y] )( p : Y => Boolean ) : Either[X,Y] = {
      src match {
        case      Left( _ )  => src;
        case r @ Right( y ) => if ( p(y) ) r else leftEmpty;
      }
    }
  }
}
