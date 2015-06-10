package com.mchange.sc.v2.util;

import org.scalacheck.{Arbitrary,Prop,Properties};
import Prop._;

// derived from scalacheck test for scala.util.Either
object CheckLeftBiasProperties extends Properties("EitherAsMonad.LeftBiased") {
  import EitherAsMonad.LeftBiased._

  property("value") = forAll((n: Int) => Left(n).get == n)

  property("getOrElse") = forAll((e: Either[Int, Int], or: Int) => e.getOrElse(or) == (e match {
    case Left(a) => a
    case Right(_) => or
  }))

  property("forall") = forAll((e: Either[Int, Int]) =>
    e.forall(_ % 2 == 0) == (e.isRight || e.get % 2 == 0))

  property("exists") = forAll((e: Either[Int, Int]) =>
    e.exists(_ % 2 == 0) == (e.isLeft && e.get % 2 == 0))

  property("flatMapLeftIdentity") = forAll((e: Either[Int, Int], n: Int, s: String) => {
    def f(x: Int) = if(x % 2 == 0) Left(s) else Right(s)
    Left(n).flatMap(f(_)) == f(n)})

  property("flatMapRightIdentity") = forAll((e: Either[Int, Int]) => e.flatMap(Left(_)) == e)

  property("flatMapComposition") = forAll((e: Either[Int, Int]) => {
    def f(x: Int) = if(x % 2 == 0) Left(x) else Right(x)
    def g(x: Int) = if(x % 7 == 0) Right(x) else Left(x)
    e.flatMap(f(_)).flatMap(g(_)) == e.flatMap(f(_).flatMap(g(_)))})

  property("mapIdentity") = forAll((e: Either[Int, Int]) => e.map(x => x) == e)

  property("mapComposition") = forAll((e: Either[String, Int]) => {
    def f(s: String) = s.toLowerCase
    def g(s: String) = s.reverse
    e.map(x => f(g(x))) == e.map(x => g(x)).map(f(_))})

  property("seq") = forAll((e: Either[Int, Int]) => e.toSeq == (e match {
    case Left(a) => Seq(a)
    case Right(_) => Seq.empty
  }))

  property("option") = forAll((e: Either[Int, Int]) => e.toOption == (e match {
    case Left(a) => Some(a)
    case Right(_) => None
  }))

  property("withFilter") = forAll((e: Either[Int, Int] ) => {
    if ( e.isLeft ) {
      if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
      else {
        try { e.withFilter( _ % 2 == 0 ); false }
        catch { case _ : NoSuchElementException => true }
      }
    } else {
      e.withFilter(_ % 2 == 0) == e // right should be unchanged
    }
  })

  property("extractTuple") = forAll((e: Either[(Int,Int,Int),Int]) => {
    if ( e.isLeft ) {
      e.get._1 == (for ( ( a, b, c ) <- e ) yield a).get
    } else {
      e == (for ( ( a, b, c ) <- e ) yield a) // right should be unchanged
    }
  })

  property("assignVariable") = forAll((e: Either[(Int,Int,Int),Int]) => {
    if ( e.isLeft ) {
      e.get._2 == (for ( tup <- e; b = tup._2 ) yield b).get
    } else {
      e == (for ( tup <- e; b = tup._2 ) yield b) // right should be unchanged
    }
  })
}



