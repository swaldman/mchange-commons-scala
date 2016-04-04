package com.mchange.sc.v2.concurrent

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import com.mchange.sc.v2.lang.borrow

object ExecutionContexts {
  def fixedThreadPool[R]( size : Int )( op : ExecutionContext => R ) = {
    borrow( ExecutionContext.fromExecutorService( Executors.newFixedThreadPool( size ) ) )( _.shutdown )( op( _ ) )
  }
}
