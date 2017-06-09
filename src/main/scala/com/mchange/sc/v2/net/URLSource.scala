package com.mchange.sc.v2.net

import scala.collection._
import scala.language.implicitConversions

import java.util.concurrent.ThreadLocalRandom
import java.net.{URL => JURL}

object URLSource {
  object URL extends URLSource[JURL] {
    def toURL( url : JURL ) : JURL = url
  }
  object String extends URLSource[String] {
    def toURL( urlStr : String ) : JURL = new JURL(urlStr)
  }
  class Seq[T : URLSource] extends URLSource[immutable.Seq[T]] {
    def toURL( seq : immutable.Seq[T] ) : JURL = {
      val src = seq( ThreadLocalRandom.current().nextInt( seq.length ) )
      implicitly[ URLSource[T] ].toURL( src )
    }
  }
  implicit def seqToSource[T : URLSource]( seq : immutable.Seq[T] ) = new URLSource.Seq[T]
}
trait URLSource[T] {
  def toURL( t : T ) : JURL
}
