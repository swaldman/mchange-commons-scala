package com.mchange.sc.v2.util;

import scala.language.implicitConversions;

import scala.util.{Either,Left,Right}

object EitherAsMonad {
  trait WithEmpty[+E] {
    def empty : E;
  }
  final object RightBiased {

    private final val OpsMethods = WithEmptyToken.Throwable( throw new MatchError( matchErrorMessage( true ) ) );   

    implicit class Ops[X,Y]( val src : Either[X,Y] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = OpsMethods.flatMap[X,XX,Y,Z]( src )( f );
      def map[Z]( f : Y => Z )                         : Either[X,Z]  = OpsMethods.map( src )( f );
      def withFilter( p : Y => Boolean )               : Either[X,Y]  = OpsMethods.withFilter( src )( p );

      // extra ops
      def exists( f : Y => Boolean )        : Boolean           = OpsMethods.exists( src )( f );
      def forall( f : Y => Boolean )        : Boolean           = OpsMethods.forall( src )( f );
      def foreach[U]( f : Y => U )          : Any               = OpsMethods.foreach( src )( f );
      def get                               : Y                 = OpsMethods.get( src );
      def getOrElse[ YY >: Y ]( or : =>YY ) : YY                = OpsMethods.getOrElse[X,Y,YY]( src )( or );
      def toOption                          : Option[Y]         = OpsMethods.toOption( src );
      def toSeq                             : collection.Seq[Y] = OpsMethods.toSeq( src );
    }

    object WithEmptyToken {
      abstract class AbstractOps[X,Y]( src : Either[X,Y] )( tc : EitherAsMonad.RightBiased.WithEmptyToken.Generic[X] ) {

        // monad ops
        def flatMap[XX >: X, Z]( f : Y => Either[XX,Z] ) : Either[XX,Z] = tc.flatMap[X,XX,Y,Z]( src )( f );
        def map[Z]( f : Y => Z )                         : Either[X,Z]  = tc.map( src )( f );
        def withFilter( p : Y => Boolean )               : Either[X,Y]  = tc.withFilter( src )( p );

        // extra ops
        def exists( f : Y => Boolean )        : Boolean           = tc.exists( src )( f );
        def forall( f : Y => Boolean )        : Boolean           = tc.forall( src )( f );
        def foreach[U]( f : Y => U )          : Any               = tc.foreach( src )( f );
        def get                               : Y                 = tc.get( src );
        def getOrElse[ YY >: Y ]( or : =>YY ) : YY                = tc.getOrElse[X,Y,YY]( src )( or );
        def toOption                          : Option[Y]         = tc.toOption( src );
        def toSeq                             : collection.Seq[Y] = tc.toSeq( src );
      }

      implicit final class Ops[X,Y]( src : Either[X,Y] )( implicit tc : EitherAsMonad.RightBiased.WithEmptyToken.Generic[X] ) extends AbstractOps( src )( tc );

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

        implicit def toOps[X>:E,Y]( src : Either[X,Y] ) : RightBiased.WithEmptyToken.Ops[X,Y] = new RightBiased.WithEmptyToken.Ops( src )( this )
      }
      def apply[E]( token : E ) : WithEmptyToken[E] = new WithEmptyToken( token );

      object Throwable {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwable = new Throwable( throwableBuilder );
      }
      final class Throwable private( throwableBuilder : =>java.lang.Throwable ) extends WithEmptyToken.Generic[Nothing] {
        override def empty : Nothing = throw throwableBuilder;
      }
    }
    final class WithEmptyToken[E] private( override val empty : E ) extends WithEmptyToken.Generic[E] {
      override protected val leftEmpty : Left[E,Nothing] = Left(empty);
    }
  }
  final object LeftBiased {
    implicit class Ops[X,Y]( val src : Either[X,Y] ) extends AnyVal {

      // monad ops
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
          case l @ Left( x )  => if ( p(x) ) l else throw new MatchError( matchErrorMessage( false, Some(src) ) );
          case     Right( _ ) => src;
        }
      }

      // extra ops
      def exists( f : X => Boolean ) : Boolean = {
        src match {
          case Left( x )  => f(x);
          case Right( _ ) => false;
        }
      }
      def forall( f : X => Boolean ) : Boolean = {
        src match {
          case Left( x )  => f(x);
          case Right( _ ) => true;
        }
      }
      def foreach[U]( f : X => U ) : Any = {
        src match {
          case Left( x )  => f(x);
          case Right( _ ) => ();
        }
      }
      def get : X = {
        src match {
          case Left( x )  => x; 
          case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
        }
      }
      def getOrElse[ XX >: X ]( or : =>XX ) : XX = {
        src match {
          case Left( x )  => x;
          case Right( _ ) => or;
        }
      }
      def toOption : Option[X] = {
        src match {
          case Left( x )  => Some( x );
          case Right( _ ) => None;
        }
      }
      def toSeq : collection.Seq[X] = {
        src match {
          case Left( x )  => collection.Seq( x );
          case Right( _ ) => collection.Seq.empty[X];
        }
      }
    }
    final object WithEmptyToken {
      implicit class Ops[X,Y]( src : Either[X,Y] )( implicit tc : EitherAsMonad.LeftBiased.WithEmptyToken[Y] ) {

        // monad ops
        def flatMap[YY >: Y, Z]( f : X => Either[Z,YY] ) : Either[Z,YY] = tc.flatMap[X,YY,Z]( src )( f );
        def map[Z]( f : X => Z )                         : Either[Z,Y]  = tc.map( src )( f );
        def withFilter( p : X => Boolean )               : Either[X,Y]  = tc.withFilter( src )( p );

        // extra ops
        def exists( f : X => Boolean )       : Boolean           = tc.exists( src )( f );
        def forall( f : X => Boolean )       : Boolean           = tc.forall( src )( f );
        def foreach[U]( f : X => U )         : Any               = tc.foreach( src )( f );
        def get                              : X                 = tc.get( src );
        def getOrElse[XX >: X ]( or : =>XX ) : XX                = tc.getOrElse[X,XX]( src )( or );
        def toOption                         : Option[X]         = tc.toOption( src );
        def toSeq                            : collection.Seq[X] = tc.toSeq( src );
      }
    }
    abstract class WithEmptyToken[Y]( val empty : Y ) extends EitherAsMonad.WithEmpty[Y]{
      val rightEmpty = Right(empty);

      // monad ops
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

      // extra ops
      def exists[X]( src : Either[X,Y] )( f : X => Boolean ) : Boolean = {
        src match {
          case Left( x )  => f(x);
          case Right( _ ) => false;
        }
      }
      def forall[X]( src : Either[X,Y] )( f : X => Boolean ) : Boolean = {
        src match {
          case Left( x )  => f(x);
          case Right( _ ) => true;
        }
      }
      def foreach[X,U]( src : Either[X,Y] )( f : X => U ) : Any = {
        src match {
          case Left( x )  => f(x);
          case Right( _ ) => ();
        }
      }
      def get[X]( src : Either[X,Y] ) : X = {
        src match {
          case Left( x )  => x; 
          case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
        }
      }
      def getOrElse[X, XX >: X ]( src : Either[X,Y] )( or : =>XX ) : XX = {
        src match {
          case Left( x )  => x;
          case Right( _ ) => or;
        }
      }
      def toOption[X]( src : Either[X,Y] ) : Option[X] = {
        src match {
          case Left( x )  => Some( x );
          case Right( _ ) => None;
        }
      }
      def toSeq[X]( src : Either[X,Y] ) : collection.Seq[X] = {
        src match {
          case Left( x )  => collection.Seq( x );
          case Right( _ ) => collection.Seq.empty[X];
        }
      }

      implicit def toOps[X]( src : Either[X,Y] ) : LeftBiased.WithEmptyToken.Ops[X,Y] = new LeftBiased.WithEmptyToken.Ops( src )( this )
    }
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

