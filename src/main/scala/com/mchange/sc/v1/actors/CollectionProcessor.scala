/*
 * Distributed as part of mchange-commons-scala
 *
 * Copyright (C) 2013 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1, as 
 * published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this software; see the file LICENSE.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

package com.mchange.sc.v1.actors;

import scala.actors.Actor;
import scala.actors.Actor._;
import scala.collection._;
import scala.collection.mutable.ArrayBuffer;

@deprecated(message = "Prefer parallel collections.")
object CollectionProcessor {
  def apply[S,T]( fcn : (S) => T ) : CollectionProcessor[S,T] = new ForForOrderRetainingCollectionProcessor[S,T]( fcn );

  def apply[S,T]( fcn : (S) => T, maxConcurrency : Int ) : CollectionProcessor[S,T] = 
    if (maxConcurrency > 0)
      new SmoothMaxConcurrencyOrderRetainingCollectionProcessor[S,T]( fcn, maxConcurrency );
    else
      this.apply( fcn );

  def dumbTest() : Unit = {
    val fcn = (cnt : Int) => { Thread.sleep( cnt ); printf("Waking %d... ", cnt); "[%d]".format( cnt ); }
    val processor = CollectionProcessor( fcn );
    val processed = processor.process( 100::200::300::400::500::600::700::800::900::1000::Nil );
    printf("All done: %s\n\n", processed.mkString(" "));
  }

  def repeatTest() : Unit = {
    var count = 1;
    while (true) {
      printf("Attempt #%d\n", count); 
      dumbTest();
      count += 1;
    }
  }

  private[actors] case object Exit;
}

@deprecated(message = "Prefer parallel collections.")
abstract class CollectionProcessor[S,T]( fcn : (S) => T ) {
  def process( items : Iterable[S] ) : Iterable[T];

  def printThrowable( t : Throwable ) {
    t.printStackTrace();
    if ( t.isInstanceOf[java.sql.SQLException] )
      printThrowable( t.asInstanceOf[java.sql.SQLException].getNextException );
  }
}

@deprecated(message = "Prefer parallel collections.")
class SmoothMaxConcurrencyOrderRetainingCollectionProcessor[S,T]( fcn : (S) => T, maxConcurrency : Int ) extends CollectionProcessor[S,T]( fcn ) {
  def process( items : Iterable[S] ) : Iterable[T] = {
    if (items.isEmpty)
      return Set.empty[T]; //code below assumes at least one item will be processed

    var index = -1;
    val inTuples = 
      (for(item <- items) yield {index += 1; index -> item}).toSeq;

    val mgr = Actor.self;
    val actors = (for (n <- 0 until maxConcurrency) yield {
      actor {
	loop {
	  react {
	    case tup : Pair[Int,S] => {
	      mgr ! ( Actor.self, tup._1, fcn( tup._2 ) );
	    }
	    case CollectionProcessor.Exit => {
	      Actor.exit();
	    }
	  }
	}
      }
    }).toSeq;

    // there's no mutable TreeMap, and trying
    // to add to and replace an immutable TreeMap
    // doesn't seem to work for reasons that still
    // confuse me.
    val out = mutable.HashMap.empty[Int,T];

    val elements = inTuples.iterator;
    
    for ( prefill <- 0 until maxConcurrency; if (elements.hasNext) ) actors(prefill) ! elements.next;

    var count = 0;
    do {
      Actor.self.receive {
	case tup : Tuple3[Actor,Int,T] => {
	  out += (tup._2 -> tup._3);
	  count += 1;
	  if ( elements.hasNext )
	    tup._1 ! elements.next;
	}
      } 
    } while ( count < inTuples.size )

    actors.foreach( _ ! CollectionProcessor.Exit );

    // since I couldn't add directly to a tree map, I sort by
    // creating a tree map... THIS ALL SEEMS INELEGANT
    (immutable.TreeMap.empty[Int,T] ++ out).map( _._2 )
  }
}

@deprecated(message = "Prefer parallel collections.")
class ChunkyMaxConcurrencyOrderRetainingCollectionProcessor[S,T]( fcn : (S) => T, maxConcurrency : Int ) extends CollectionProcessor[S,T]( fcn ) {
  def process( items : Iterable[S] ) : Iterable[T] = {
    val out = new ArrayBuffer[T];
    val processor = CollectionProcessor[S,T]( fcn );    

    val elements = items.iterator;
    while( elements.hasNext )
    {
      val inChunk = new ArrayBuffer[S];
      for (i <- 0 until maxConcurrency; if (elements.hasNext)) {
	inChunk += elements.next;
      } 
      out ++= processor.process( inChunk );
    }
    out;
  }
}

@deprecated(message = "Prefer parallel collections.")
class ForForOrderRetainingCollectionProcessor[S,T]( fcn : (S) => T ) extends CollectionProcessor[S,T]( fcn ) {
/* 
  Let's hope this is fixed by Scala 2.9...

  System.err.println(
    """|TEMPORY/DEBUG: Under some circumstances, ForForOrderRetainingCollectionProcessor.process() has
       |been associated with hard-to-debug stack overflow / infinite recursion like...
       |
       |java.lang.StackOverflowError
       |   at scala.runtime.BoxesRunTime.equals(Unknown Source)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   at scala.$colon$colon.hashCode(List.scala:1376)
       |   at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:90)
       |   ...
       |
       |I'm unsure whether this is a Scala 2.7.6 bug, or something wrong with this code. A workaround
       |is to use SmoothMaxConcurrencyOrderRetainingCollectionProcessor (the default CollectionProcessor
       |with maxConcurrency set.)""".stripMargin('|')
  );

*/ 

  def process( items : Iterable[S] ) /* ( implicit manifest : Manifest[T] ) */ : Iterable[T] = {
    var cnt = 0;
    val mgr : Actor = Actor.self;

    var actors : List[Actor] = Nil; 

    def makeActor() = actor {
      react {
	case CollectionProcessor.Exit => Actor.exit();
	case s : S => {
	  try { 
	    val processed = fcn(s);
	    mgr ! (s, processed);
	  } catch  { 
	    case t : Throwable => {
	      t.printStackTrace();
	      mgr ! t;
	    }
	  }
	}
      }
    };

    // will only be called by Thread calling process
    // unproblematic to access local vars here
    def selfReceive() = Actor.self.receive[Pair[S,T]] {
      case trouble : Throwable => {
	trouble.printStackTrace();
	actors.foreach( _ ! CollectionProcessor.Exit );
	throw trouble;
      }
      case (s : S, t : T) => {
	(s, t)
      }
      case msg @ _ => { 
	throw new Exception("unexpected message during gathering: %s\n".format(msg));
      }
    }
    
    var started  = 0;
    val indexMap = mutable.Map.empty[S,Int];
    for ( item <- items ) {
      val a = makeActor() 
      actors = a::actors;
      a ! item;
      indexMap += (item -> started); //we index prior to increment, since we want zero-based indices
      started += 1;
      printf("processors started and messaged: %s\n", started);
    }

    /*
    val out = new ArrayBuffer[T](started);
    
    println( "started: " + started );
    println( "ArrayBuffer: " + out );
    */

    val out = mutable.HashMap.empty[Int,T];

    for ( i <- 0 until started ) {
      val ( item, processed ) = selfReceive();
      out( indexMap(item) ) = processed;
      printf("processors finished: %s\n", i + 1);
    };

    (immutable.TreeMap.empty[Int,T] ++ out).map( _._2 ).toSeq;
  }
}


// this implementation is problematic because it seems
// like map is sometimes implemented lazily, so the timing
// of side effects in the mapping function can be unpredictable
//
// plus, it fails to offer the processed objects back in
// an order consistent with the corresponding precursor objects

@deprecated(message = "Prefer parallel collections.")
class MapMapCollectionProcessor[S,T]( fcn : (S) => T ) extends CollectionProcessor[S,T]( fcn ) {
  def process( items : Iterable[S] ) : Iterable[T] = {
    var cnt = 0;
    val mgr : Actor = Actor.self;
    val actors = items.map {
      item => {
	val a : Actor = actor {
	  react {
	    case CollectionProcessor.Exit => Actor.exit();
	    case s : S => {
	      try { 
		val processed = fcn(s);
		mgr ! processed;
	      } catch  { 
		case t : Throwable => {
		  t.printStackTrace();
		  mgr ! t;
		}
	      }
	    }
	    case x @ _ => throw new Exception("unexpected message to item processor (expected %s): %s".format( item, x));
	  }
	};
	cnt +=1;
	a ! item;
	printf("process actor %s started and sent %s!!! [%d processors started.]\n", a, item, cnt);
	a
      }
    };

    //printf("Actors: %s\n", actors.mkString("[ ", ", " ," ]")); 
    //printf("All actors [%d] started\n.", cnt);
    
    actors.map {
      a => {
	Actor.self.receive[T] {
	  case trouble : Throwable => {
	    trouble.printStackTrace();
	    actors.foreach( _ ! CollectionProcessor.Exit );
	    throw trouble;
	  }
	  case t : T => {
	    cnt -= 1; printf("process actor finished!!! [%d processors still active.]\n", cnt);
	    t
	  }
	  case msg @ _ => { //I guess due to type erasure the above will always match, so this never will... alas.
	    throw new Exception("unexpected message during gathering: %s\n".format(msg));
	  }
	}
      }
    }
  }
}

class CollectionProcessorException(message : String, cause : Throwable) extends Exception( message, cause ) {
  def this( message : String ) = this( message, null );
  def this( cause : Throwable ) = this( null, cause );
  def this() = this( null, null );
}

