package com.mchange.sc.v2;

import java.io.File;
import com.typesafe.config.Config;
import com.mchange.v3.hocon.HoconUtils;

import scala.collection.JavaConversions._
import com.mchange.sc.v1.log.MLevel._

package object hocon {
  private implicit val logger = mlogger( this )

  def customFileOrSpecifiedSourceWins( customFile : File ) : Config = {
    val out = HoconUtils.customFileOrSpecifiedSourceWins( customFile )
    out.warnings.foreach( WARNING.log( _ ) )
    out.config
  }
}
