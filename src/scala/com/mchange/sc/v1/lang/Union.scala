package com.mchange.sc.v1.lang;

object Union {
  // i think that, as a practical matter, this is probably not so useful

  // Miles Sabin's uboxed union type hack... see http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/
  type ¬[A] = A => Nothing;
  type ¬¬[A] = ¬[¬[A]];
  type v[T, U] = ¬[¬[T] with ¬[U]];
  type ~[T, U] = { type union[X] = ¬¬[X] <:< (T v U) };

  // def check[T : (String~Int)#union ]( arg : T ) = arg
}
