package com.mchange.sc.v2.net

import java.net.{URL => JURL}

object URLSource {
  def toURL[T : URLSource]( t : T ) = implicitly[URLSource[T]].toURL( t )

  implicit object URL extends URLSource[JURL] {
    def toURL( url : JURL ) : JURL = url
  }
  implicit object String extends URLSource[String] {
    def toURL( urlStr : String ) : JURL = new JURL(urlStr)
  }
}
trait URLSource[T] {
  def toURL( t : T ) : JURL
}
