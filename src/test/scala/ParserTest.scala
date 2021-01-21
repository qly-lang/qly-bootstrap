import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class ParserTest extends FreeSpec {
  "Parse atoms" - {
    "Parse Integers" in {
      QlyParser("1").toString shouldEqual "(1)"
      QlyParser("0").toString shouldEqual "(0)"
      QlyParser("-1").toString shouldEqual "(-1)"
      QlyParser("+15").toString shouldEqual "(15)"
      QlyParser(BigInt(2).pow(129).toString).toString shouldEqual "("+BigInt(2).pow(129).toString+")"
    }

    "Integer type" in {
      QlyParser("0").mexps.head.getClass shouldEqual classOf[QlyInt32]
      QlyParser(Int.MaxValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt32]
      QlyParser(Int.MinValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt32]
      QlyParser(Long.MaxValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt64]
      QlyParser(Long.MinValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt64]
      QlyParser((BigInt(2).pow(127)-1).toString).mexps.head.getClass shouldEqual classOf[QlyInt128]
      QlyParser((-BigInt(2).pow(127)).toString).mexps.head.getClass shouldEqual classOf[QlyInt128]
      QlyParser((BigInt(2).pow(127)).toString).mexps.head.getClass shouldEqual classOf[QlyBigInt]
      QlyParser((-BigInt(2).pow(127)-1).toString).mexps.head.getClass shouldEqual classOf[QlyBigInt]
    }

    "Parse UInt" in {
      QlyParser("0x0").toString shouldEqual "(0x0)"
      QlyParser("0o0").toString shouldEqual "(0o0)"
      QlyParser("0b0").toString shouldEqual "(0b0)"
      QlyParser("0xff").toString shouldEqual "(0xff)"
      QlyParser("0o377").toString shouldEqual "(0o377)"
      QlyParser("0b11111111").toString shouldEqual "(0b11111111)"
    }

    "UInt type" in {
      QlyParser("0x0").mexps.head.getClass shouldEqual classOf[QlyUInt32]
      QlyParser("0o0").mexps.head.getClass shouldEqual classOf[QlyUInt32]
      QlyParser("0b0").mexps.head.getClass shouldEqual classOf[QlyUInt32]
      QlyParser("0x" + (BigInt(2).pow(32)-1).toString(16)).mexps.head.getClass shouldEqual classOf[QlyUInt32]
      QlyParser("0x" + (BigInt(2).pow(32)).toString(16)).mexps.head.getClass shouldEqual classOf[QlyUInt64]
      QlyParser("0o" + (BigInt(2).pow(64)-1).toString(8)).mexps.head.getClass shouldEqual classOf[QlyUInt64]
      QlyParser("0o" + (BigInt(2).pow(64)).toString(8)).mexps.head.getClass shouldEqual classOf[QlyUInt128]
      QlyParser("0b" + (BigInt(2).pow(128)-1).toString(2)).mexps.head.getClass shouldEqual classOf[QlyUInt128]
      QlyParser("0b" + (BigInt(2).pow(128)).toString(2)).mexps.head.getClass shouldEqual classOf[QlyBigUInt]
    }

    "Parse real" in {
      QlyParser("0.0").toString shouldEqual "(0.0)"
      QlyParser("0.").toString shouldEqual "(0.0)"
      QlyParser(".0").toString shouldEqual "(0.0)"
      QlyParser("1.5").toString shouldEqual "(1.5)"
      QlyParser("-1.5").toString shouldEqual "(-1.5)"
      QlyParser("+1.5").toString shouldEqual "(1.5)"
      QlyParser("1.5e0").toString shouldEqual "(1.5)"
      QlyParser("1.5E0").toString shouldEqual "(1.5)"
      QlyParser("0.15e+1").toString shouldEqual "(1.5)"
      QlyParser("150.0e-2").toString shouldEqual "(1.5)"
      QlyParser("150e-2").toString shouldEqual "(1.5)"
    }

    "Parse string" in {
      QlyParser(""""a"""").toString shouldEqual("""("a")""")
      QlyParser(""""ab"""").toString shouldEqual("""("ab")""")
      QlyParser(
        """"a
          |b"""".stripMargin).toString shouldEqual(
        """("a
          |b")""".stripMargin)
      QlyParser(
        """"a\"""""
      ).toString shouldEqual("""("a\"")""")
      QlyParser(
        """"a\\n""""
      ).toString shouldEqual("""("a\\n")""")
      QlyParser("""""""").toString shouldEqual("""("")""")
      the [QlyParserError] thrownBy(QlyParser("""aa""""))
      the [QlyParserError] thrownBy(QlyParser(""""aa"""))
    }

    "Parse atom" in {
      QlyParser("a").toString shouldEqual("(a)")
      QlyParser("1a").toString shouldEqual("(|1a|)")
      QlyParser("1.3a").toString shouldEqual("((. 1 |3a|))")
      QlyParser("1 . 3a").toString shouldEqual("((. 1 |3a|))")
      QlyParser("1 .3a").toString shouldEqual("((. 1 |3a|))")
      QlyParser("1. 3a").toString shouldEqual("(1.0 |3a|)")
      QlyParser("1e2e3").toString shouldEqual("(|1e2e3|)")
      QlyParser("15e-2x").toString shouldEqual("(|15e-2x|)")
      QlyParser("1.5e-2x").toString shouldEqual("((. 1 |5e-2x|))")
      QlyParser("0x3.2").toString shouldEqual("((. 0x3 2))")
      QlyParser("-").toString shouldEqual("(-)")
      QlyParser("""\1""").toString shouldEqual("(|1|)")
      QlyParser("""\1\.5\e2""").toString shouldEqual("(|1.5e2|)")
      QlyParser("""a bbb""").toString shouldEqual("(a bbb)")
      QlyParser("""a\ bbb""").toString shouldEqual("(|a bbb|)")
      QlyParser(
        """a\
          |bbb""".stripMargin).toString shouldEqual(
        """(|a
          |bbb|)""".stripMargin)
      QlyParser(
        """a\#b"""
      ).toString shouldEqual("(|a#b|)")
      QlyParser("""\,\.\'""").toString shouldEqual("(|,.'|)")
    }
  }

  "Parse MExp" - {
    "Parse comment" in {
      QlyParser("#").toString shouldEqual("()")
      QlyParser("#a").toString shouldEqual("()")
      QlyParser(
        """#a
          |3""".stripMargin).toString shouldEqual("(3)")
      QlyParser("3#a").toString shouldEqual("(3)")
      QlyParser("3 #aaa").toString shouldEqual("(3)")
      QlyParser(
        """3#a
          |4
          |""".stripMargin).toString shouldEqual("(3 4)")
      QlyParser(""""aaa#3"""").toString shouldEqual("""("aaa#3")""")
      QlyParser(
        """#aaa
          |#bbb
          |""".stripMargin).toString shouldEqual("()")
    }

    "Parse array" in {
      QlyParser("[]").toString shouldEqual("((array))")
      QlyParser("[ ]").toString shouldEqual("((array))")
      QlyParser("[a 1]").toString shouldEqual("((array a 1))")
      QlyParser("[ a 1]").toString shouldEqual("((array a 1))")
      QlyParser("[a 1 ]").toString shouldEqual("((array a 1))")
      QlyParser("[ a 1 ]").toString shouldEqual("((array a 1))")
      QlyParser("[a [1]]").toString shouldEqual("((array a (array 1)))")
    }

    "Parse quote, unquote and splice" in {
      QlyParser("'a").toString shouldEqual("((' a))")
      QlyParser("''a").toString shouldEqual("((' (' a)))")
      the [QlyParserError] thrownBy(QlyParser("'"))
      QlyParser("' ,a").toString shouldEqual("((' (, a)))")
      QlyParser("',a").toString shouldEqual("((' (, a)))")
      QlyParser("', a").toString shouldEqual("((' (, a)))")
      QlyParser("' , a").toString shouldEqual("((' (, a)))")
      QlyParser("'@[a]").toString shouldEqual("((' (@ (array a))))")
    }

    "Parse dot exp" in {
      QlyParser("aaa.bbb").toString shouldEqual("((. aaa bbb))")
      QlyParser("aaa .bbb").toString shouldEqual("((. aaa bbb))")
      QlyParser("aaa. bbb").toString shouldEqual("((. aaa bbb))")
      QlyParser("aaa . bbb").toString shouldEqual("((. aaa bbb))")
      QlyParser("aaa.bbb.ccc").toString shouldEqual("((. (. aaa bbb) ccc))")
      QlyParser("a.b[].c").toString shouldEqual("((. ((. a b)) c))")
      QlyParser("aaa.bbb[x y.ddd].ccc").toString shouldEqual("((. ((. aaa bbb) x (. y ddd)) ccc))")
      QlyParser("a.b[d e][x].f[]").toString shouldEqual("(((. (((. a b) d e) x) f)))")
      QlyParser("aaa.b ccc").toString shouldEqual("((. aaa b) ccc)")
    }

    "Parse call exp" in {
      QlyParser("a[b c 3 \"str\"]").toString shouldEqual("""((a b c 3 "str"))""")
      QlyParser("a[]").toString shouldEqual("((a))")
      QlyParser("a[b c][d]").toString shouldEqual("(((a b c) d))")
    }

    "Parse colon exp" in {
      QlyParser("a:b").toString shouldEqual("((: a b))")
      QlyParser("a:b:c").toString shouldEqual("((: a (: b c)))")
      QlyParser("a.b:c").toString shouldEqual("((: (. a b) c))")
      QlyParser("a:b.c").toString shouldEqual("((: a (. b c)))")
      QlyParser("a:b[]").toString shouldEqual("((: a (b)))")
      QlyParser("a[]:b").toString shouldEqual("((: (a) b))")
      QlyParser("f[foo [a:int b:array[string]]:type-a]").toString shouldEqual("((f foo (: (array (: a int) (: b (array string))) type-a)))")
    }

    "Parse MExps" in {
      QlyParser(
        """a[]
          |b[]
          |c.d""".stripMargin).toString shouldEqual("((a) (b) (. c d))")
    }
  }
}
