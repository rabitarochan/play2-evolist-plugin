package com.rabitarochan.evolist

case class EvolistOpts (
  val dbname: String
)

object EvolistOpts {
  def default = EvolistOpts( "default" )

  def apply( args: Seq[String] ): EvolistOpts = {
    def loop( opt: EvolistOpts, xs: Seq[String] ): Option[EvolistOpts] = xs match {
      case "-db" :: dbname :: l => loop( opt.copy( dbname = dbname ), l )
      case Nil                  => Some( opt )
      case _                    => None
    }

    loop( default, args ) getOrElse default
  }
}