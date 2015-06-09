package com.mchange.sc.v2.util;

import scala.language.implicitConversions;

import scala.util.{Either,Left,Right}

object EitherAsMonad {
  trait WithEmpty[+E] {
    def empty : E;
  }
  final object RightBiased {

    private[EitherAsMonad] final val DefaultThrowingOps = WithEmptyToken.Throwing( throw new MatchError( matchErrorMessage( true ) ) );

    implicit class Ops[A,B]( val src : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = DefaultThrowingOps.flatMap[A,AA,B,Z]( src )( f );
      def map[Z]( f : B => Z )                         : Either[A,Z]  = DefaultThrowingOps.map( src )( f );
      def withFilter( p : B => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( src )( p );

      // extra ops
      def exists( f : B => Boolean )        : Boolean           = DefaultThrowingOps.exists( src )( f );
      def forall( f : B => Boolean )        : Boolean           = DefaultThrowingOps.forall( src )( f );
      def foreach[U]( f : B => U )          : Any               = DefaultThrowingOps.foreach( src )( f );
      def get                               : B                 = DefaultThrowingOps.get( src );
      def getOrElse[ BB >: B ]( or : =>BB ) : BB                = DefaultThrowingOps.getOrElse[A,B,BB]( src )( or );
      def toOption                          : Option[B]         = DefaultThrowingOps.toOption( src );
      def toSeq                             : collection.Seq[B] = DefaultThrowingOps.toSeq( src );

      def fold[Z]( ifLeft : A => Z )( ifRight : B => Z ) : Z = DefaultThrowingOps.fold( src )( ifLeft )( ifRight )
    }

    object WithEmptyToken {
      abstract class AbstractOps[A,B]( src : Either[A,B] )( opsTypeClass : EitherAsMonad.RightBiased.WithEmptyToken.Generic[A] ) {

        // monad ops
        def flatMap[AA >: A, Z]( f : B => Either[AA,Z] ) : Either[AA,Z] = opsTypeClass.flatMap[A,AA,B,Z]( src )( f );
        def map[Z]( f : B => Z )                         : Either[A,Z]  = opsTypeClass.map( src )( f );
        def withFilter( p : B => Boolean )               : Either[A,B]  = opsTypeClass.withFilter( src )( p );

        // extra ops
        def exists( f : B => Boolean )        : Boolean           = opsTypeClass.exists( src )( f );
        def forall( f : B => Boolean )        : Boolean           = opsTypeClass.forall( src )( f );
        def foreach[U]( f : B => U )          : Any               = opsTypeClass.foreach( src )( f );
        def get                               : B                 = opsTypeClass.get( src );
        def getOrElse[ BB >: B ]( or : =>BB ) : BB                = opsTypeClass.getOrElse[A,B,BB]( src )( or );
        def toOption                          : Option[B]         = opsTypeClass.toOption( src );
        def toSeq                             : collection.Seq[B] = opsTypeClass.toSeq( src );

        def fold[Z]( ifLeft : A => Z )( ifRight : B => Z ) : Z = opsTypeClass.fold( src )( ifLeft )( ifRight )
      }

      implicit final class Ops[A,B]( src : Either[A,B] )( implicit opsTypeClass : EitherAsMonad.RightBiased.WithEmptyToken.Generic[A] ) extends AbstractOps( src )( opsTypeClass );

      trait Generic[+E] extends EitherAsMonad.WithEmpty[E]{
        protected def leftEmpty : Left[E,Nothing] = Left(empty);

        // monad ops
        def flatMap[A>:E,AA>:A,B,Z]( src : Either[A,B] )( f : B => Either[AA,Z] ) : Either[AA,Z] = {
          src match {
            case Left( _ )  => src.asInstanceOf[Left[A,Z]]
            case Right( b ) => f( b )
          }
        }
        def map[A>:E,B,Z]( src : Either[A,B] )( f : B => Z ) : Either[A,Z] = {
          src match {
            case Left( _ )  => src.asInstanceOf[Left[A,Z]]
            case Right( b ) => Right( f( b ) )
          }
        }
        def withFilter[A>:E,B]( src : Either[A,B] )( p : B => Boolean ) : Either[A,B] = {
          src match {
            case      Left( _ ) => src;
            case r @ Right( b ) => if ( p(b) ) r else leftEmpty;
          }
        }

        // extra ops
        def exists[A>:E,B]( src : Either[A,B] )( f : B => Boolean ) : Boolean = {
          src match {
            case Left( _ )  => false;
            case Right( b ) => f( b );
          }
        }
        def forall[A>:E,B]( src : Either[A,B] )( f : B => Boolean ) : Boolean = {
          src match {
            case Left( _ )  => true;
            case Right( b ) => f( b );
          }
        }
        def foreach[A>:E,B,U]( src : Either[A,B] )( f : B => U ) : Any = {
          src match {
            case Left( _ )  => ();
            case Right( b ) => f( b );
          }
        }
        def get[A>:E,B]( src : Either[A,B] ) : B = {
          src match {
            case Left( _ )  => throw new NoSuchElementException( NoSuchRightMessage );
            case Right( b ) => b;
          }
        }
        def getOrElse[A>:E,B,BB>:B]( src : Either[A,B] )( or : =>BB ) : BB = {
          src match {
            case Left( _ )  => or;
            case Right( b ) => b;
          }
        }
        def toOption[A>:E,B]( src : Either[A,B] ) : Option[B] = {
          src match {
            case Left( _ )  => None;
            case Right( b ) => Some( b );
          }
        }
        def toSeq[A>:E,B]( src : Either[A,B] ) : collection.Seq[B] = {
          src match {
            case Left( _ )  => collection.Seq.empty[B];
            case Right( b ) => collection.Seq( b );
          }
        }
        def fold[A>:E,B,Z]( src : Either[A,B] )( ifLeft : A => Z )( ifRight : B => Z ) : Z = {
          src match {
            case Left( a ) => ifLeft( a );
            case Right( b ) => ifRight( b );
          }
        }

        implicit def toOps[A>:E,B]( src : Either[A,B] ) : RightBiased.WithEmptyToken.Ops[A,B] = new RightBiased.WithEmptyToken.Ops[A,B]( src )( this )
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
  trait RightBiased[A] {
    val EmptyTokenDefinition : EitherAsMonad.RightBiased.WithEmptyToken.Generic[A] = RightBiased.DefaultThrowingOps;

    implicit def toRightBiasedEtherOps[B]( src : Either[A,B] ) : RightBiased.WithEmptyToken.AbstractOps[A,B] = new RightBiased.WithEmptyToken.Ops[A,B]( src )( EmptyTokenDefinition );
  }
  final object LeftBiased {

    private[EitherAsMonad] final val DefaultThrowingOps = WithEmptyToken.Throwing( throw new MatchError( matchErrorMessage( false ) ) );

    implicit class Ops[A,B]( val src : Either[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = DefaultThrowingOps.flatMap[A,B,BB,Z]( src )( f );
      def map[Z]( f : A => Z )                         : Either[Z,B]  = DefaultThrowingOps.map( src )( f );
      def withFilter( p : A => Boolean )               : Either[A,B]  = DefaultThrowingOps.withFilter( src )( p );

      // extra ops
      def exists( f : A => Boolean )       : Boolean           = DefaultThrowingOps.exists( src )( f );
      def forall( f : A => Boolean )       : Boolean           = DefaultThrowingOps.forall( src )( f );
      def foreach[U]( f : A => U )         : Any               = DefaultThrowingOps.foreach( src )( f );
      def get                              : A                 = DefaultThrowingOps.get( src );
      def getOrElse[AA >: A ]( or : =>AA ) : AA                = DefaultThrowingOps.getOrElse[A,AA,B]( src )( or );
      def toOption                         : Option[A]         = DefaultThrowingOps.toOption( src );
      def toSeq                            : collection.Seq[A] = DefaultThrowingOps.toSeq( src );
   
      def fold[Z]( ifLeft : A => Z )( ifRight : B => Z ) : Z = DefaultThrowingOps.fold( src )( ifLeft )( ifRight )
    }

    object WithEmptyToken {
      abstract class AbstractOps[A,B]( src : Either[A,B] )( opsTypeClass : EitherAsMonad.LeftBiased.WithEmptyToken.Generic[B] ) {

        // monad ops
        def flatMap[BB >: B, Z]( f : A => Either[Z,BB] ) : Either[Z,BB] = opsTypeClass.flatMap[A,B,BB,Z]( src )( f );
        def map[Z]( f : A => Z )                         : Either[Z,B]  = opsTypeClass.map( src )( f );
        def withFilter( p : A => Boolean )               : Either[A,B]  = opsTypeClass.withFilter( src )( p );

        // extra ops
        def exists( f : A => Boolean )       : Boolean           = opsTypeClass.exists( src )( f );
        def forall( f : A => Boolean )       : Boolean           = opsTypeClass.forall( src )( f );
        def foreach[U]( f : A => U )         : Any               = opsTypeClass.foreach( src )( f );
        def get                              : A                 = opsTypeClass.get( src );
        def getOrElse[AA >: A ]( or : =>AA ) : AA                = opsTypeClass.getOrElse[A,AA,B]( src )( or );
        def toOption                         : Option[A]         = opsTypeClass.toOption( src );
        def toSeq                            : collection.Seq[A] = opsTypeClass.toSeq( src );

        def fold[Z]( ifLeft : A => Z )( ifRight : B => Z ) : Z = opsTypeClass.fold( src )( ifLeft )( ifRight )
      }

      implicit final class Ops[A,B]( src : Either[A,B] )( implicit opsTypeClass : EitherAsMonad.LeftBiased.WithEmptyToken.Generic[B] ) extends AbstractOps( src )( opsTypeClass );

      trait Generic[+E] extends EitherAsMonad.WithEmpty[E]{
        protected def rightEmpty : Right[Nothing,E] = Right(empty);

        // monad ops
        def flatMap[A, B>:E, BB>:B ,Z]( src : Either[A,B] )( f : A => Either[Z,BB] ) : Either[Z,BB] = {
          src match {
            case Left( a )  => f( a )
            case Right( _ ) => src.asInstanceOf[Right[Z,B]]
          }
        }
        def map[A, B>:E, Z]( src : Either[A,B] )( f : A => Z ) : Either[Z,B] = {
          src match {
            case Left( a )  => Left( f( a ) )
            case Right( _ ) => src.asInstanceOf[Right[Z,B]]
          }
        }
        def withFilter[A,B>:E]( src : Either[A,B] )( p : A => Boolean ) : Either[A,B] = {
          src match {
            case l @  Left( a ) => if ( p(a) ) l else rightEmpty;
            case     Right( _ ) => src;
          }
        }

        // extra ops
        def exists[A,B>:E]( src : Either[A,B] )( f : A => Boolean ) : Boolean = {
          src match {
            case Left( a )  => f(a);
            case Right( _ ) => false;
          }
        }
        def forall[A,B>:E]( src : Either[A,B] )( f : A => Boolean ) : Boolean = {
          src match {
            case Left( a )  => f(a)
            case Right( _ ) => true;
          }
        }
        def foreach[A,B>:E,U]( src : Either[A,B] )( f : A => U ) : Any = {
          src match {
            case Left( a )  => f(a);
            case Right( _ ) => ();
          }
        }
        def get[A,B>:E]( src : Either[A,B] ) : A = {
          src match {
            case Left( a )  => a;
            case Right( _ ) => throw new NoSuchElementException( NoSuchLeftMessage );
          }
        }
        def getOrElse[A, AA>:A, B>:E]( src : Either[A,B] )( or : =>AA ) : AA = {
          src match {
            case Left( a )  => a;
            case Right( _ ) => or;
          }
        }
        def toOption[A,B>:E]( src : Either[A,B] ) : Option[A] = {
          src match {
            case Left( a )  => Some( a );
            case Right( _ ) => None; 
          }
        }
        def toSeq[A,B>:E]( src : Either[A,B] ) : collection.Seq[A] = {
          src match {
            case Left( a )  => collection.Seq( a );
            case Right( _ ) => collection.Seq.empty[A];
          }
        }
        def fold[A,B>:E,Z]( src : Either[A,B] )( ifLeft : A => Z )( ifRight : B => Z ) : Z = {
          src match {
            case Left( a ) => ifLeft( a );
            case Right( b ) => ifRight( b );
          }
        }

        implicit def toOps[A,B>:E]( src : Either[A,B] ) : LeftBiased.WithEmptyToken.Ops[A,B] = new LeftBiased.WithEmptyToken.Ops[A,B]( src )( this )
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
  trait LeftBiased[B] {
    val EmptyTokenDefinition : EitherAsMonad.LeftBiased.WithEmptyToken.Generic[B] = LeftBiased.DefaultThrowingOps;

    implicit def toLeftBiasedEtherOps[A]( src : Either[A,B] ) : LeftBiased.WithEmptyToken.AbstractOps[A,B] = new LeftBiased.WithEmptyToken.Ops[A,B]( src )( EmptyTokenDefinition );
  }

  private def matchErrorMessage[A,B]( rightBiased : Boolean, mbEither : Option[Either[A,B]] = None ) = {
    val bias = if ( rightBiased ) "Right-biased" else "Left-biased";
    val withToken = if ( rightBiased ) "RightBiased.WithEmptyToken" else "LeftBiased.WithEmptyToken";
    val eitherRep = mbEither.fold(" ")( either => s" '${either}' " );
    s"${bias} Either${eitherRep}filtered to empty or failed to match a pattern. Consider implementing ${withToken}"
  }

  private val NoSuchLeftMessage = "Can't get a value from a LeftBiased Either which is in fact a Right.";
  private val NoSuchRightMessage = "Can't get a value from a RightBiased Either which is in fact a Left.";
}

