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
          case r @ Right( y ) => if ( p(y) ) r else throw new MatchError( matchErrorMessage( true, src ) );
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
  final object LeftBiased {
    implicit class Ops[X,Y]( val src : Either[X,Y] ) extends AnyVal {
      def flatMap[YY >: Y, Z]( f : X => Either[Z,YY] ) : Either[Z,YY] = {
        src match {
          case Left( x )  => f( x )
          case Right( _ ) => src.asInstanceOf[Right[Z,Y]]
        }
      }
      def map[Z]( f : X => Z ) : Either[Z,Y] = {
        src match {
          case Left( x )  => Left( f( x ) )
          case Right( _ ) => src.asInstanceOf[Right[Z,Y]]
        }
      }
      def withFilter( p : X => Boolean ) : Either[X,Y] = {
        src match {
          case l @ Left( x )  => if ( p(x) ) l else throw new MatchError( matchErrorMessage( false, src ) );
          case     Right( _ ) => src;
        }
      }
    }
    final object WithEmptyToken {
      implicit class Ops[X,Y]( src : Either[X,Y] )( implicit tc : EitherAsMonad.LeftBiased.WithEmptyToken[Y] ) {
        def flatMap[YY >: Y, Z]( f : X => Either[Z,YY] ) : Either[Z,YY] = tc.flatMap[X,YY,Z]( src )( f );
        def map[Z]( f : X => Z ) : Either[Z,Y] = tc.map( src )( f );
        def withFilter( src : Either[X,Y] )( p : X => Boolean ) : Either[X,Y] = tc.withFilter( src )( p );
      }
    }
    abstract class WithEmptyToken[Y]( val empty : Y ) extends EitherAsMonad.WithEmptyToken[Y]{
      val rightEmpty = Right(empty);

      def flatMap[X, YY >: Y, Z]( src : Either[X,Y] )( f : X => Either[Z,YY] ) : Either[Z,YY] = {
        src match {
          case Left( x )  => f( x )
          case Right( _ ) => src.asInstanceOf[Right[Z,YY]]
        }
      }
      def map[X,Z]( src : Either[X,Y] )( f : X => Z ) : Either[Z,Y] = {
        src match {
          case Left( x )  => Left( f( x ) )
          case Right( _ ) => src.asInstanceOf[Right[Z,Y]]
        }
      }
      def withFilter[X]( src : Either[X,Y] )( p : X => Boolean ) : Either[X,Y] = {
        src match {
          case l @ Left( x )  => if ( p(x) ) l else rightEmpty;
          case     Right( _ ) => src;
        }
      }
      implicit def toOps[X]( src : Either[X,Y] ) : LeftBiased.WithEmptyToken.Ops[X,Y] = new LeftBiased.WithEmptyToken.Ops( src )( this )
    }
  }

  private def matchErrorMessage[X,Y]( rightBiased : Boolean, either : Either[X,Y] ) = {
    val bias = if ( rightBiased ) "Right-biased" else "Left-biased";
    val withToken = if ( rightBiased ) "RightBiased.WithToken" else "LeftBiased.WithToken";
    s"${bias} Either '${either}' filtered or would have no value as unmatched pattern. Consider implementing ${withToken}"
  }
}

