package com.mchange.sc.v1.util;

import java.io._;
import scala.io.Source;
import scala.io.Codec;

import com.mchange.v2.io.FileUtils.findRelativeToParent;
import ClosableUtils.withClosable;

object LicenseHeaderRewriter {

  private val CRLF = util.Properties.lineSeparator;

  def rewriteSrcDir( srcDir : File, targetDir : File, filter : (File) => Boolean, licenseHeader : String, headerConverter : (String) => String, headerLineWhile : (String) => Boolean )( implicit codec : Codec ) : Unit = {
    require( srcDir.isDirectory, "Source dir %s is not a directory.".format( srcDir ) );
    require( targetDir.isDirectory, "Target dir %s is not a directory.".format( srcDir ) );

    // thanks to http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
    def findAllChildren( dir : File ) : Seq[File] = {
      val kids = dir.listFiles();
      kids ++ kids.filter( _.isDirectory ).flatMap( findAllChildren( _ ) ) 
    }

    val srcFiles = findAllChildren( srcDir ).filter( srcFile => ( (!srcFile.isDirectory) && filter(_) ) );
    srcFiles.foreach { 
      file => {
	val relFile = findRelativeToParent(srcDir, file);
	val destFile = new File( targetDir, relFile.getPath );
	val text = rewritten( file, licenseHeader, headerConverter, headerLineWhile )( codec );
	withClosable( () => new BufferedWriter( new OutputStreamWriter( new FileOutputStream( destFile ), codec.encoder ) ) ) {
	  writer => {
	    writer.write( text );
	    writer.flush();
	  }
	}
      }
    }
  }


  def rewritten( file : File, licenseHeader : String, headerConverter : (String) => String, headerLineWhile : (String) => Boolean )(implicit codec : Codec) : String = 
    rewritten( Source.fromFile( file )( codec ).getLines, licenseHeader, headerConverter, headerLineWhile )

  def rewritten( lines : Iterator[String], licenseHeader : String, headerConverter : (String) => String, headerLineWhile : (String) => Boolean ) : String = {
    lines.dropWhile( _.trim.length == 0 ); // drop empty lines
    lines.dropWhile( headerLineWhile );
    headerConverter( licenseHeader ) + lines.mkString( CRLF ) + CRLF
  }

  def identityHeaderConverter( licenseHeader : String ) = CRLF + licenseHeader.trim + CRLF;

  def starSurroundHeaderConverter( licenseHeader : String ) : String = {
    """[\r\n]+""".r.split( licenseHeader.trim ).map(" * " + _).mkString(CRLF+"""/*"""+CRLF, CRLF, CRLF + " */"+CRLF)
  }

  def starSurroundHeaderLineWhile( line : String ) : Boolean = {
    val trimmed = line.trim;
    trimmed.startsWith("/*") || trimmed.startsWith("*") || trimmed.startsWith("*/");
  }
  
}
