import java.nio.file.Path
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Reader}

case class QlySyntaxError(location: Location, msg: String)
    extends QlyCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"line: $line, column: $column"
}

object QlyParser extends RegexParsers with PackratParsers {
  override def skipWhitespace: Boolean = false

  // TODO: human understandable error message
  @throws(classOf[QlySyntaxError])
  def apply(code: String): AST = {
    val reader = new PackratReader(new CharSequenceReader(code))
    phrase(mexps)(reader) match {
      case NoSuccess(msg, next) => {
        println(next.pos)
        println(msg)
        throw QlySyntaxError(Location(next.pos.line, next.pos.column), msg)
      }
      case Success(result, next) => new AST(result)
    }
  }

  @throws(classOf[QlySyntaxError])
  def apply(path: Path): AST = {
    val file = scala.io.Source.fromFile(path.toString)
    val result = apply(file.mkString)
    file.close()
    result
  }

  def mexps: Parser[List[MExp]] = {
    whitespaceAndMexp.* <~ whitespace.?
  }

  def whitespaceAndMexp: Parser[MExp] = {
    whitespace.? ~> mexp
  }

  lazy val mexp: PackratParser[MExp] = positioned {
    colonExpAndHigher
  }

  lazy val colonExpAndHigher: PackratParser[MExp] = positioned {
    colonExp | callDotExpAndHigher
  }

  lazy val colonExp: PackratParser[ColonExp] = positioned {
    callDotExpAndHigher ~ (whitespace.? ~> ':' <~ whitespace.?) ~ colonExpAndHigher ^^ {
      case v ~ _ ~ c => ColonExp(v, c)
    }
  }

  lazy val callDotExpAndHigher: PackratParser[MExp] = positioned {
    callExp | dotExp | primaryExp
  }

  lazy val callExp: PackratParser[CallExp] = positioned {
    callDotExpAndHigher ~ qlyArray ^^ {
      case v ~ a => CallExp(v, a.value)
    }
  }

  lazy val dotExp: PackratParser[DotExp] = positioned {
    callDotExpAndHigher ~ (whitespace.? ~> '.' <~ whitespace.?) ~ primaryExp ^^ {
      case v ~ _ ~ d => DotExp(v, d)
    }
  }

  def primaryExp: Parser[MExp] =
    positioned {
      quoteExp | unquoteExp | spliceExp | qlyArray | atom
    }

  def qlyArray: Parser[QlyArray] =
    positioned {
      '[' ~ mexps ~ ']' ^^ {
        case _ ~ elems ~ _ => QlyArray(elems)
      }
    }

  def quoteExp: Parser[QuoteExp] =
    positioned {
      '\'' ~ whitespaceAndMexp ^^ {
        case _ ~ m => QuoteExp(m)
      }
    }

  def unquoteExp: Parser[UnquoteExp] =
    positioned {
      ',' ~ whitespaceAndMexp ^^ {
        case _ ~ m => UnquoteExp(m)
      }
    }

  def spliceExp: Parser[SpliceExp] =
    positioned {
      '@' ~ whitespaceAndMexp ^^ {
        case _ ~ m => SpliceExp(m)
      }
    }

  def atom: Parser[Atom] =
    positioned {
      qlyString | qlyReal | qlyUInt | qlyInt | qlySymbol
    }

  def qlyString: Parser[QlyString] =
    positioned {
      """"((\\[\d\D])|([^"]))*"""".r ^^ { s =>
        new QlyString(s.drop(1).dropRight(1).replaceAll("""\\([\d\D])""", "$1"))
      }
    }

  def qlyReal: Parser[QlyReal] =
    positioned {
      """[-+]?([0-9]*[.][0-9]*([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)""".r <~ guard(
        not(symbolChar)
      ) ^^ { s =>
        {
          val d = BigDecimal(s)
          if (d.isExactDouble) QlyFloat64(d.toDouble) else QlyDecimal(d)
        }
      }
    }

  def qlyInt: Parser[QlyInt] =
    positioned {
      """[-+]?[0-9]+""".r <~ guard(not(symbolChar)) ^^ { s =>
        {
          val i = BigInt(s)
          if (i.isValidInt)
            QlyInt32(i.toInt)
          else if (i.isValidLong)
            QlyInt64(i.toLong)
          else if (i < BigInt(2).pow(127) && i >= -BigInt(2).pow(127))
            QlyInt128(i)
          else
            QlyBigInt(i)
        }
      }
    }

  def qlyUInt: Parser[QlyUInt] =
    positioned {
      """(0x[0-9a-f]+)|(0o[0-7]+)|(0b[01]+)""".r <~ guard(not(symbolChar)) ^^ {
        s =>
          {
            val b = s.drop(1).head
            val base = if (b == 'x') 16 else if (b == 'o') 8 else 2
            val u = BigInt(s.drop(2), base)
            if (u < BigInt(2).pow(32)) {
              QlyUInt32(u, base)
            } else if (u < BigInt(2).pow(64)) {
              QlyUInt64(u, base)
            } else if (u < BigInt(2).pow(128)) {
              QlyUInt128(u, base)
            } else {
              QlyBigUInt(u, base)
            }
          }
      }
    }

  def qlySymbol: Parser[QlySymbol] =
    positioned {
      symbolChar.+ ^^ { s =>
        QlySymbol(s.mkString.replaceAll("""\\([\d\D])""", "$1"))
      }
    }

  def symbolChar: Parser[Char] = {
    """\\[\d\D]""".r ^^ { escapeChar =>
      escapeChar.charAt(1)
    } | """[^\s.\[\]:"\\',@#]""".r ^^ { char =>
      char.head
    }
  }

  def whitespace: Parser[Unit] = {
    ("""[\s]""".r | comment).+ ^^^ ()
  }

  def comment: Parser[Unit] = {
    """#.*""".r ~ '\n'.? ^^^ ()
  }
}
