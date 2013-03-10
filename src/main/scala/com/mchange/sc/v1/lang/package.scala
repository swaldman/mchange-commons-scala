package com.mchange.sc.v1;

package object lang {

  /**
   * Intended to mark things defined as a Scala Singletin objects,
   * i.e. things defines as like "object Foo { ... }"
   * 
   * Unfortunately, the compiler can't enforce this.
   */ 
  type SingletonObject = AnyRef;
}

