package com.mchange.sc.v2.util;

import scala.language.implicitConversions;

import scala.util.{Either,Left,Right}

object EitherAsMonad {

  trait WithEmpty[+T] {
    def empty : T;
  }
  final object RightBiased {
    final object WithEmpty {
      implicit class Ops[X,Y]( src : Either[X,Y] )( implicit tc : EitherAsMonad.RightBiased.WithEmpty[X] ) {
        def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = tc.flatMap[XX,Y,Z]( src )( f );
        def map[Z]( f : Y => Z ) : Either[X,Z] = tc.map( src )( f );
        def withFilter( src : Either[X,Y] )( p : Y => Boolean ) : Either[X,Y] = tc.withFilter( src )( p );
      }
    }
    abstract class WithEmpty[X]( val empty : X ) extends EitherAsMonad.WithEmpty[X]{
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
          case      Left( _ )  => src;
          case r @ Right( y ) => if ( p(y) ) r else leftEmpty;
        }
      }
      implicit def toOps[Y]( src : Either[X,Y] ) : RightBiased.WithEmpty.Ops[X,Y] = new RightBiased.WithEmpty.Ops( src )( this )
    }
    final object WithoutEmpty {
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
    }
  }
}
