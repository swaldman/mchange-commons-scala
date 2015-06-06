package com.mchange.sc.v2.util;

import scala.language.implicitConversions;

import scala.util.{Either,Left,Right}

object EitherAsMonad {
  trait WithEmptyToken[+T] {
    def empty : T;
  }
  final object RightBiased {
    implicit class Ops[X,Y]( val src : Either[X,Y] ) extends AnyVal {
      def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = {
        src match {
          case Left( _ )  => src.asInstanceOf[Left[X,Z]]
          case Right( y ) => f( y )
        }
      }
      def map[Z]( f : Y => Z ) : Either[X,Z] = {
        src match {
          case Left( _ )  => src.asInstanceOf[Left[X,Z]]
          case Right( y ) => Right( f( y ) )
        }
      }
      def withFilter( p : Y => Boolean ) : Either[X,Y] = {
        src match {
          case      Left( _ )  => src;
          case r @ Right( y ) => if ( p(y) ) r else throw new MatchError( s"Either filtered or as unmatched pattern would have no value" )
        }
      }
    }
    final object WithEmptyToken {
      implicit class Ops[X,Y]( src : Either[X,Y] )( implicit tc : EitherAsMonad.RightBiased.WithEmptyToken[X] ) {
        def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = tc.flatMap[XX,Y,Z]( src )( f );
        def map[Z]( f : Y => Z ) : Either[X,Z] = tc.map( src )( f );
        def withFilter( src : Either[X,Y] )( p : Y => Boolean ) : Either[X,Y] = tc.withFilter( src )( p );
      }
    }
    abstract class WithEmptyToken[X]( val empty : X ) extends EitherAsMonad.WithEmptyToken[X]{
      val leftEmpty = Left(empty);

      def flatMap[XX >: X, Y, Z]( src : Either[X,Y] )( f : Y => Either[XX,Z] ) : Either[XX,Z] = {
        src match {
          case Left( _ )  => src.asInstanceOf[Left[X,Z]]
          case Right( y ) => f( y )
        }
      }
      def map[Y,Z]( src : Either[X,Y] )( f : Y => Z ) : Either[X,Z] = {
        src match {
          case Left( _ )  => src.asInstanceOf[Left[X,Z]]
          case Right( y ) => Right( f( y ) )
        }
      }
      def withFilter[Y]( src : Either[X,Y] )( p : Y => Boolean ) : Either[X,Y] = {
        src match {
          case      Left( _ ) => src;
          case r @ Right( y ) => if ( p(y) ) r else leftEmpty;
        }
      }
      implicit def toOps[Y]( src : Either[X,Y] ) : RightBiased.WithEmptyToken.Ops[X,Y] = new RightBiased.WithEmptyToken.Ops( src )( this )
    }
  }
}

