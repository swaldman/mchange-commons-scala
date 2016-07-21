package com.mchange.sc.v2.util

import java.io.File

import com.mchange.sc.v2.failable._

import com.mchange.sc.v1.log.MLevel._

object Platform {
  private val Tag = "Platform"

  private lazy implicit val logger = mlogger( this )

  final case object Mac extends Platform {
    lazy val appSupportParentDirectory : Failable[File] = {
      Option( System.getProperty("user.home") ).map( home => new java.io.File( s"${home}/Library/Application Support" ) ).toFailable(s"${Tag}: On Mac, but could not find System property 'user.home'")
    }
    def appSupportDirectory( appName : String ) : Failable[File] = appSupportParentDirectory.map( new File( _, appName ) )

    def UserLibraryDirectory = appSupportParentDirectory
  }
  final case object Unix extends Platform {
    def appSupportDirectory( appName : String ) : Failable[File] = {
      Option( System.getProperty("user.home") ).map( home => new java.io.File( s"${home}/.${appName}" ) ).toFailable("${Tag}: On Unix, but could not find System property 'user.home'")
    }
  }
  final case object Windows extends Platform {
    lazy val appSupportParentDirectory : Failable[File] = {
      Option( System.getenv("APPDATA") ).map( new java.io.File(_) ).toFailable("${Tag}: On Windows, but could not find environment variable 'APPDATA'")
    }
    def appSupportDirectory( appName : String ) : Failable[File] = appSupportParentDirectory.map( new File( _, appName ) )
  }

  lazy val Current : Option[Platform] = {
    val mbOsName = Option( System.getProperty("os.name") ).map( _.toLowerCase ).xwarn(s"${Tag}: Couldn't detect OS, System property 'os.name' not available.")

    mbOsName flatMap { osName =>
      if ( osName.indexOf( "mac" ) >= 0 )        Some( Mac )
      else if ( osName.indexOf( "win" ) >= 0 )   Some( Windows )
      else if ( osName.indexOf( "linux" ) >= 0 ) Some( Unix )
      else if ( osName.indexOf( "bsd" ) >= 0 )   Some( Unix )
      else if ( osName.indexOf( "sunos" ) >= 0 ) Some( Unix )
      else if ( osName.indexOf( "unix" ) >= 0 )  Some( Unix )
      else                                       None
    }
  }
}
trait Platform { // NOT sealed, as the universe of platforms may expand over time!
  def appSupportDirectory( appName : String ) : Failable[File]
}
