package com.rabitarochan.evolist

import sbt._
import java.io.{ File, FileInputStream }
import scala.io.{ Source }
import scala.util.matching.{ Regex }

case class Evolution( revision: Int, comment: String )

object EvolistPlugin extends Plugin {
  
  override lazy val settings = Seq( Keys.commands += evolistCommand )

  def evolistCommand = Command.args( "evolist", "<args>" ) { ( state, args ) =>
    val opts = EvolistOpts( args )

    val evolutions = getEvolutions( opts.dbname )

    val headerIndent = evolutions.length.toString.length
    val separator = ": "

    val formatWithIndent = PrintFormatter.format( _: String, headerIndent, separator, _: String, _: Int )

    println( "database: %s".format( opts.dbname ) )
    evolutions foreach { evolution => 
      formatWithIndent( evolution.revision.toString, evolution.comment, 80 ) foreach println
    }

    state
  }

  def getEvolutions( dbname: String ): List[Evolution] = {
    def isMatch( line: String, r: Regex ): Boolean = !( r.findAllIn( line ).isEmpty )
    val commentMarker = """#[ \t-]*(.*)$""".r
    val isMatchUps = isMatch( _: String, """^.*!Ups.*$""".r )
    val isMatchDowns = isMatch( _: String, """^.*!Downs.*$""".r )

    val scripts = Collections.unfoldLeft( 1 ) { revision =>
      val scriptPath = "conf/evolutions/" + dbname + "/" + revision + ".sql"
      Option( new File( scriptPath ) ).filter( _.exists ).map {
        Source.fromFile(_).getLines.toList.map( _.trim )
      }.map { script =>
        ( revision + 1, ( revision, script ) )
      }
    }

    scripts.sortBy( _._1 ).map {
      case ( revision, script ) => {
        val markerLines = script.filter( _.startsWith( "#" ))

        val comment = markerLines.filter( !isMatchUps(_) ).filter( !isMatchDowns(_) ).flatMap { _ match {
          case commentMarker( s ) => List( s )
          case _ => Nil
        }}

        Evolution( revision, comment.mkString( " " ) )
      }
    }
  }

}

object Collections {
  def unfoldLeft[TKey, TResult]( key: TKey )( f: TKey => Option[(TKey, TResult)] ): List[TResult] = {
    def loop( k: TKey )( xs: List[TResult] ): List[TResult] = f( k ) match {
      case Some(( a, b )) => loop( a )( b :: xs )
      case None           => xs
    }

    loop( key )( Nil )
  }
}

object PrintFormatter {
  def format( header: String, headerIndent: Int, separator: String,
              value: String, column: Int ): List[String] = {
    val indent = headerIndent + separator.length
    val valueLength = column - indent
    val headerFormat = "%" + indent + "s"

    val v = value match {
      case "" => "<no comments>"
      case _  => value
    }

    def loop( h: String, v: Option[String], xs: List[String] ): List[String] = v match {
      case None => xs
      case Some( s ) => s match {
        case ss if ss.length > column => loop( headerFormat.format( "" ), Some( ss.substring( valueLength ) ), xs :+ ( h + ss.take( valueLength ) ) ) 
        case _                        => loop( headerFormat.format( "" ), None, xs :+ ( h + s ) )
      }
    }

    loop( headerFormat.format( header + separator ), Some( v ), Nil)
  }
}
