package com.mchange.sc.v2;

import scala.util.{Either,Left,Right}

package object util {
  implicit class EitherAsMonadRightBiasedOps[X,Y]( src : Either[X,Y] )( implicit tc : EitherAsMonad.RightBiased[X,Y] ) {
    def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = tc.flatMap[XX,Z]( src )( f );
    def map[Z]( f : Y => Z ) : Either[X,Z] = tc.map( src )( f );
    def withFilter( src : Either[X,Y] )( p : Y => Boolean ) : Either[X,Y] = tc.withFilter( src )( p );
  }
}
