package com.mchange.sc.v2.util;

import scala.language.implicitConversions;

import scala.util.{Either,Left,Right}

object EitherAsMonad {
  trait WithEmpty[+E] {
    def empty : E;
  }
  final object RightBiased {

    private[EitherAsMonad] final val DefaultThrowingOps = WithEmptyToken.Throwing( throw new MatchError( matchErrorMessage( true ) ) );

    implicit class Ops[X,Y]( val src : Either[X,Y] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = DefaultThrowingOps.flatMap[X,XX,Y,Z]( src )( f );
      def map[Z]( f : Y => Z )                         : Either[X,Z]  = DefaultThrowingOps.map( src )( f );
      def withFilter( p : Y => Boolean )               : Either[X,Y]  = DefaultThrowingOps.withFilter( src )( p );

      // extra ops
      def exists( f : Y => Boolean )        : Boolean           = DefaultThrowingOps.exists( src )( f );
      def forall( f : Y => Boolean )        : Boolean           = DefaultThrowingOps.forall( src )( f );
      def foreach[U]( f : Y => U )          : Any               = DefaultThrowingOps.foreach( src )( f );
      def get                               : Y                 = DefaultThrowingOps.get( src );
      def getOrElse[ YY >: Y ]( or : =>YY ) : YY                = DefaultThrowingOps.getOrElse[X,Y,YY]( src )( or );
      def toOption                          : Option[Y]         = DefaultThrowingOps.toOption( src );
      def toSeq                             : collection.Seq[Y] = DefaultThrowingOps.toSeq( src );

      def fold[Z]( ifLeft : X => Z )( ifRight : Y => Z ) : Z = DefaultThrowingOps.fold( src )( ifLeft )( ifRight )
    }

    object WithEmptyToken {
      abstract class AbstractOps[X,Y]( src : Either[X,Y] )( opsTypeClass : EitherAsMonad.RightBiased.WithEmptyToken.Generic[X] ) {

        // monad ops
        def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = opsTypeClass.flatMap[X,XX,Y,Z]( src )( f );
        def map[Z]( f : Y => Z )                         : Either[X,Z]  = opsTypeClass.map( src )( f );
        def withFilter( p : Y => Boolean )               : Either[X,Y]  = opsTypeClass.withFilter( src )( p );

        // extra ops
        def exists( f : Y => Boolean )        : Boolean           = opsTypeClass.exists( src )( f );
        def forall( f : Y => Boolean )        : Boolean           = opsTypeClass.forall( src )( f );
        def foreach[U]( f : Y => U )          : Any               = opsTypeClass.foreach( src )( f );
        def get                               : Y                 = opsTypeClass.get( src );
        def getOrElse[ YY >: Y ]( or : =>YY ) : YY                = opsTypeClass.getOrElse[X,Y,YY]( src )( or );
        def toOption                          : Option[Y]         = opsTypeClass.toOption( src );
        def toSeq                             : collection.Seq[Y] = opsTypeClass.toSeq( src );

        def fold[Z]( ifLeft : X => Z )( ifRight : Y => Z ) : Z = opsTypeClass.fold( src )( ifLeft )( ifRight )
      }

      implicit final class Ops[X,Y]( src : Either[X,Y] )( implicit opsTypeClass : EitherAsMonad.RightBiased.WithEmptyToken.Generic[X] ) extends AbstractOps( src )( opsTypeClass );

      trait Generic[+E] extends EitherAsMonad.WithEmpty[E]{
        protected def leftEmpty : Left[E,Nothing] = Left(empty);

        // monad ops
        def flatMap[X>:E,XX>:X,Y,Z]( src : Either[X,Y] )( f : Y => Either[XX,Z] ) : Either[XX,Z] = {
          src match {
            case Left( _ )  => src.asInstanceOf[Left[X,Z]]
            case Right( y ) => f( y )
          }
        }
        def map[X>:E,Y,Z]( src : Either[X,Y] )( f : Y => Z ) : Either[X,Z] = {
          src match {
            case Left( _ )  => src.asInstanceOf[Left[X,Z]]
            case Right( y ) => Right( f( y ) )
          }
        }
        def withFilter[X>:E,Y]( src : Either[X,Y] )( p : Y => Boolean ) : Either[X,Y] = {
          src match {
            case      Left( _ ) => src;
            case r @ Right( y ) => if ( p(y) ) r else leftEmpty;
          }
        }

        // extra ops
        def exists[X>:E,Y]( src : Either[X,Y] )( f : Y => Boolean ) : Boolean = {
          src match {
            case Left( _ )  => false;
            case Right( y ) => f( y );
          }
        }
        def forall[X>:E,Y]( src : Either[X,Y] )( f : Y => Boolean ) : Boolean = {
          src match {
            case Left( _ )  => true;
            case Right( y ) => f( y );
          }
        }
        def foreach[X>:E,Y,U]( src : Either[X,Y] )( f : Y => U ) : Any = {
          src match {
            case Left( _ )  => ();
            case Right( y ) => f( y );
          }
        }
        def get[X>:E,Y]( src : Either[X,Y] ) : Y = {
          src match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchRightMessage );
            case Right( y ) => y;
          }
        }
        def getOrElse[X>:E,Y,YY>:Y]( src : Either[X,Y] )( or : =>YY ) : YY = {
          src match {
            case Left( _ )  => or;
            case Right( y ) => y;
          }
        }
        def toOption[X>:E,Y]( src : Either[X,Y] ) : Option[Y] = {
          src match {
            case Left( _ )  => None;
            case Right( y ) => Some( y );
          }
        }
        def toSeq[X>:E,Y]( src : Either[X,Y] ) : collection.Seq[Y] = {
          src match {
            case Left( _ )  => collection.Seq.empty[Y];
            case Right( y ) => collection.Seq( y );
          }
        }
        def fold[X>:E,Y,Z]( src : Either[X,Y] )( ifLeft : X => Z )( ifRight : Y => Z ) : Z = {
          src match {
            case Left( x ) => ifLeft( x );
            case Right( y ) => ifRight( y );
          }
        }

        implicit def toOps[X>:E,Y]( src : Either[X,Y] ) : RightBiased.WithEmptyToken.Ops[X,Y] = new RightBiased.WithEmptyToken.Ops[X,Y]( src )( this )
      }
      def apply[E]( token : E ) : WithEmptyToken[E] = new WithEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends WithEmptyToken.Generic[Nothing] {
        override def empty : Nothing = throw throwableBuilder;
      }
    }
    final class WithEmptyToken[+E] private( override val empty : E ) extends WithEmptyToken.Generic[E] {
      override protected val leftEmpty : Left[E,Nothing] = Left(empty);
    }
  }
  trait RightBiased[X] {
    val EmptyTokenDefinition : EitherAsMonad.RightBiased.WithEmptyToken.Generic[X] = RightBiased.DefaultThrowingOps;

    implicit def toRightBiasedEtherOps[Y]( src : Either[X,Y] ) : RightBiased.WithEmptyToken.AbstractOps[X,Y] = new RightBiased.WithEmptyToken.Ops[X,Y]( src )( EmptyTokenDefinition );
  }
  final object LeftBiased {

    private[EitherAsMonad] final val DefaultThrowingOps = WithEmptyToken.Throwing( throw new MatchError( matchErrorMessage( false ) ) );

    implicit class Ops[X,Y]( val src : Either[X,Y] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[YY >: Y, Z]( f : X => Either[Z,YY] ) : Either[Z,YY] = DefaultThrowingOps.flatMap[X,Y,YY,Z]( src )( f );
      def map[Z]( f : X => Z )                         : Either[Z,Y]  = DefaultThrowingOps.map( src )( f );
      def withFilter( p : X => Boolean )               : Either[X,Y]  = DefaultThrowingOps.withFilter( src )( p );

      // extra ops
      def exists( f : X => Boolean )       : Boolean           = DefaultThrowingOps.exists( src )( f );
      def forall( f : X => Boolean )       : Boolean           = DefaultThrowingOps.forall( src )( f );
      def foreach[U]( f : X => U )         : Any               = DefaultThrowingOps.foreach( src )( f );
      def get                              : X                 = DefaultThrowingOps.get( src );
      def getOrElse[XX >: X ]( or : =>XX ) : XX                = DefaultThrowingOps.getOrElse[X,XX,Y]( src )( or );
      def toOption                         : Option[X]         = DefaultThrowingOps.toOption( src );
      def toSeq                            : collection.Seq[X] = DefaultThrowingOps.toSeq( src );
   
      def fold[Z]( ifLeft : X => Z )( ifRight : Y => Z ) : Z = DefaultThrowingOps.fold( src )( ifLeft )( ifRight )
    }

    object WithEmptyToken {
      abstract class AbstractOps[X,Y]( src : Either[X,Y] )( opsTypeClass : EitherAsMonad.LeftBiased.WithEmptyToken.Generic[Y] ) {

        // monad ops
        def flatMap[YY >: Y, Z]( f : X => Either[Z,YY] ) : Either[Z,YY] = opsTypeClass.flatMap[X,Y,YY,Z]( src )( f );
        def map[Z]( f : X => Z )                         : Either[Z,Y]  = opsTypeClass.map( src )( f );
        def withFilter( p : X => Boolean )               : Either[X,Y]  = opsTypeClass.withFilter( src )( p );

        // extra ops
        def exists( f : X => Boolean )       : Boolean           = opsTypeClass.exists( src )( f );
        def forall( f : X => Boolean )       : Boolean           = opsTypeClass.forall( src )( f );
        def foreach[U]( f : X => U )         : Any               = opsTypeClass.foreach( src )( f );
        def get                              : X                 = opsTypeClass.get( src );
        def getOrElse[XX >: X ]( or : =>XX ) : XX                = opsTypeClass.getOrElse[X,XX,Y]( src )( or );
        def toOption                         : Option[X]         = opsTypeClass.toOption( src );
        def toSeq                            : collection.Seq[X] = opsTypeClass.toSeq( src );

        def fold[Z]( ifLeft : X => Z )( ifRight : Y => Z ) : Z = opsTypeClass.fold( src )( ifLeft )( ifRight )
      }

      implicit final class Ops[X,Y]( src : Either[X,Y] )( implicit opsTypeClass : EitherAsMonad.LeftBiased.WithEmptyToken.Generic[Y] ) extends AbstractOps( src )( opsTypeClass );

      trait Generic[+E] extends EitherAsMonad.WithEmpty[E]{
        protected def rightEmpty : Right[Nothing,E] = Right(empty);

        // monad ops
        def flatMap[X, Y>:E, YY>:Y ,Z]( src : Either[X,Y] )( f : X => Either[Z,YY] ) : Either[Z,YY] = {
          src match {
            case Left( x )  => f( x )
            case Right( _ ) => src.asInstanceOf[Right[Z,Y]]
          }
        }
        def map[X, Y>:E, Z]( src : Either[X,Y] )( f : X => Z ) : Either[Z,Y] = {
          src match {
            case Left( x )  => Left( f( x ) )
            case Right( _ ) => src.asInstanceOf[Right[Z,Y]]
          }
        }
        def withFilter[X,Y>:E]( src : Either[X,Y] )( p : X => Boolean ) : Either[X,Y] = {
          src match {
            case l @  Left( x ) => if ( p(x) ) l else rightEmpty;
            case     Right( y ) => src;
          }
        }

        // extra ops
        def exists[X,Y>:E]( src : Either[X,Y] )( f : X => Boolean ) : Boolean = {
          src match {
            case Left( x )  => f(x);
            case Right( y ) => false;
          }
        }
        def forall[X,Y>:E]( src : Either[X,Y] )( f : X => Boolean ) : Boolean = {
          src match {
            case Left( x )  => f(x)
            case Right( _ ) => true;
          }
        }
        def foreach[X,Y>:E,U]( src : Either[X,Y] )( f : X => U ) : Any = {
          src match {
            case Left( x )  => f(x);
            case Right( _ ) => ();
          }
        }
        def get[X,Y>:E]( src : Either[X,Y] ) : X = {
          src match {
            case Left( x )  => x;
            case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
          }
        }
        def getOrElse[X, XX>:X, Y>:E]( src : Either[X,Y] )( or : =>XX ) : XX = {
          src match {
            case Left( x )  => x;
            case Right( _ ) => or;
          }
        }
        def toOption[X,Y>:E]( src : Either[X,Y] ) : Option[X] = {
          src match {
            case Left( x )  => Some( x );
            case Right( _ ) => None; 
          }
        }
        def toSeq[X,Y>:E]( src : Either[X,Y] ) : collection.Seq[X] = {
          src match {
            case Left( x )  => collection.Seq( x );
            case Right( _ ) => collection.Seq.empty[X];
          }
        }
        def fold[X,Y>:E,Z]( src : Either[X,Y] )( ifLeft : X => Z )( ifRight : Y => Z ) : Z = {
          src match {
            case Left( x ) => ifLeft( x );
            case Right( y ) => ifRight( y );
          }
        }

        implicit def toOps[X,Y>:E]( src : Either[X,Y] ) : LeftBiased.WithEmptyToken.Ops[X,Y] = new LeftBiased.WithEmptyToken.Ops[X,Y]( src )( this )
      }
      def apply[E]( token : E ) : WithEmptyToken[E] = new WithEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends WithEmptyToken.Generic[Nothing] {
        override def empty : Nothing = throw throwableBuilder;
      }
    }
    final class WithEmptyToken[+E] private( override val empty : E ) extends WithEmptyToken.Generic[E] {
      override protected val rightEmpty : Right[Nothing,E] = Right(empty);
    }
  } 
  trait LeftBiased[Y] {
    val EmptyTokenDefinition : EitherAsMonad.LeftBiased.WithEmptyToken.Generic[Y] = LeftBiased.DefaultThrowingOps;

    implicit def toLeftBiasedEtherOps[X]( src : Either[X,Y] ) : LeftBiased.WithEmptyToken.AbstractOps[X,Y] = new LeftBiased.WithEmptyToken.Ops[X,Y]( src )( EmptyTokenDefinition );
  }

  private def matchErrorMessage[X,Y]( rightBiased : Boolean, mbEither : Option[Either[X,Y]] = None ) = {
    val bias = if ( rightBiased ) "Right-biased" else "Left-biased";
    val withToken = if ( rightBiased ) "RightBiased.WithEmptyToken" else "LeftBiased.WithEmptyToken";
    val eitherRep = mbEither.fold(" ")( either => s" '${either}' " );
    s"${bias} Either${eitherRep}filtered to empty or failed to match a pattern. Consider implementing ${withToken}"
  }

  private val NoSuchLeftMessage = "Can't get a value from a LeftBiased Either which is in fact a Right.";
  private val NoSuchRightMessage = "Can't get a value from a RightBiased Either which is in fact a Left.";
}

