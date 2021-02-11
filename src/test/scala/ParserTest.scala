import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class ParserTest extends FreeSpec {
  "Parse atoms" - {
    "Parse Integers" in {
      Parser("1").toString shouldEqual "(1)"
      Parser("0").toString shouldEqual "(0)"
      Parser("-1").toString shouldEqual "(-1)"
      Parser("+15").toString shouldEqual "(15)"
      Parser(BigInt(2).pow(129).toString).toString shouldEqual "(" + BigInt(2).pow(129).toString + ")"
    }

    "Integer type" in {
      Parser("0").mexps.head.getClass shouldEqual classOf[QlyInt32]
      Parser(Int.MaxValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt32]
      Parser(Int.MinValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt32]
      Parser(Long.MaxValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt64]
      Parser(Long.MinValue.toString).mexps.head.getClass shouldEqual classOf[QlyInt64]
      Parser((BigInt(2).pow(127) - 1).toString).mexps.head.getClass shouldEqual classOf[QlyInt128]
      Parser((-BigInt(2).pow(127)).toString).mexps.head.getClass shouldEqual classOf[QlyInt128]
      Parser((BigInt(2).pow(127)).toString).mexps.head.getClass shouldEqual classOf[QlyBigInt]
      Parser((-BigInt(2).pow(127) - 1).toString).mexps.head.getClass shouldEqual classOf[QlyBigInt]
    }

    "Parse UInt" in {
      Parser("0x0").toString shouldEqual "(0x0)"
      Parser("0o0").toString shouldEqual "(0o0)"
      Parser("0b0").toString shouldEqual "(0b0)"
      Parser("0xff").toString shouldEqual "(0xff)"
      Parser("0o377").toString shouldEqual "(0o377)"
      Parser("0b11111111").toString shouldEqual "(0b11111111)"
    }

    "UInt type" in {
      Parser("0x0").mexps.head.getClass shouldEqual classOf[QlyUInt32]
      Parser("0o0").mexps.head.getClass shouldEqual classOf[QlyUInt32]
      Parser("0b0").mexps.head.getClass shouldEqual classOf[QlyUInt32]
      Parser("0x" + (BigInt(2).pow(32) - 1).toString(16)).mexps.head.getClass shouldEqual classOf[QlyUInt32]
      Parser("0x" + (BigInt(2).pow(32)).toString(16)).mexps.head.getClass shouldEqual classOf[QlyUInt64]
      Parser("0o" + (BigInt(2).pow(64) - 1).toString(8)).mexps.head.getClass shouldEqual classOf[QlyUInt64]
      Parser("0o" + (BigInt(2).pow(64)).toString(8)).mexps.head.getClass shouldEqual classOf[QlyUInt128]
      Parser("0b" + (BigInt(2).pow(128) - 1).toString(2)).mexps.head.getClass shouldEqual classOf[QlyUInt128]
      Parser("0b" + (BigInt(2).pow(128)).toString(2)).mexps.head.getClass shouldEqual classOf[QlyBigUInt]
    }

    "Parse real" in {
      Parser("0.0").toString shouldEqual "(0.0)"
      Parser("0.").toString shouldEqual "(0.0)"
      Parser(".0").toString shouldEqual "(0.0)"
      Parser("1.5").toString shouldEqual "(1.5)"
      Parser("-1.5").toString shouldEqual "(-1.5)"
      Parser("+1.5").toString shouldEqual "(1.5)"
      Parser("1.5e0").toString shouldEqual "(1.5)"
      Parser("1.5E0").toString shouldEqual "(1.5)"
      Parser("0.15e+1").toString shouldEqual "(1.5)"
      Parser("150.0e-2").toString shouldEqual "(1.5)"
      Parser("150e-2").toString shouldEqual "(1.5)"
    }

    "Parse string" in {
      Parser(""""a"""").toString shouldEqual ("""("a")""")
      Parser(""""ab"""").toString shouldEqual ("""("ab")""")
      Parser(""""a
          |b"""".stripMargin).toString shouldEqual ("""("a
          |b")""".stripMargin)
      Parser(
        """"a\"""""
      ).toString shouldEqual ("""("a\"")""")
      Parser(
        """"a\\n""""
      ).toString shouldEqual ("""("a\\n")""")
      Parser("""""""").toString shouldEqual ("""("")""")
      the[SyntaxError] thrownBy (Parser("""aa""""))
      the[SyntaxError] thrownBy (Parser(""""aa"""))
    }

    "Parse atom" in {
      Parser("a").toString shouldEqual ("(a)")
      Parser("1a").toString shouldEqual ("(|1a|)")
      Parser("1.3a").toString shouldEqual ("((. 1 |3a|))")
      Parser("1 . 3a").toString shouldEqual ("((. 1 |3a|))")
      Parser("1 .3a").toString shouldEqual ("((. 1 |3a|))")
      Parser("1. 3a").toString shouldEqual ("(1.0 |3a|)")
      Parser("1e2e3").toString shouldEqual ("(|1e2e3|)")
      Parser("15e-2x").toString shouldEqual ("(|15e-2x|)")
      Parser("1.5e-2x").toString shouldEqual ("((. 1 |5e-2x|))")
      Parser("0x3.2").toString shouldEqual ("((. 0x3 2))")
      Parser("-").toString shouldEqual ("(-)")
      Parser("""\1""").toString shouldEqual ("(|1|)")
      Parser("""\1\.5\e2""").toString shouldEqual ("(|1.5e2|)")
      Parser("""a bbb""").toString shouldEqual ("(a bbb)")
      Parser("""a\ bbb""").toString shouldEqual ("(|a bbb|)")
      Parser("""a\
          |bbb""".stripMargin).toString shouldEqual ("""(|a
          |bbb|)""".stripMargin)
      Parser(
        """a\#b"""
      ).toString shouldEqual ("(|a#b|)")
      Parser("""\,\.\'""").toString shouldEqual ("(|,.'|)")
    }
  }

  "Parse MExp" - {
    "Parse comment" in {
      Parser("#").toString shouldEqual ("()")
      Parser("#a").toString shouldEqual ("()")
      Parser("""#a
          |3""".stripMargin).toString shouldEqual ("(3)")
      Parser("3#a").toString shouldEqual ("(3)")
      Parser("3 #aaa").toString shouldEqual ("(3)")
      Parser("""3#a
          |4
          |""".stripMargin).toString shouldEqual ("(3 4)")
      Parser(""""aaa#3"""").toString shouldEqual ("""("aaa#3")""")
      Parser("""#aaa
          |#bbb
          |""".stripMargin).toString shouldEqual ("()")
    }

    "Parse array" in {
      Parser("[]").toString shouldEqual ("((array))")
      Parser("[ ]").toString shouldEqual ("((array))")
      Parser("[a 1]").toString shouldEqual ("((array a 1))")
      Parser("[ a 1]").toString shouldEqual ("((array a 1))")
      Parser("[a 1 ]").toString shouldEqual ("((array a 1))")
      Parser("[ a 1 ]").toString shouldEqual ("((array a 1))")
      Parser("[a [1]]").toString shouldEqual ("((array a (array 1)))")
    }

    "Parse quote, unquote and splice" in {
      Parser("'a").toString shouldEqual ("((' a))")
      Parser("''a").toString shouldEqual ("((' (' a)))")
      the[SyntaxError] thrownBy (Parser("'"))
      Parser("' ,a").toString shouldEqual ("((' (, a)))")
      Parser("',a").toString shouldEqual ("((' (, a)))")
      Parser("', a").toString shouldEqual ("((' (, a)))")
      Parser("' , a").toString shouldEqual ("((' (, a)))")
      Parser("'@[a]").toString shouldEqual ("((' (@ (array a))))")
    }

    "Parse dot exp" in {
      Parser("aaa.bbb").toString shouldEqual ("((. aaa bbb))")
      Parser("aaa .bbb").toString shouldEqual ("((. aaa bbb))")
      Parser("aaa. bbb").toString shouldEqual ("((. aaa bbb))")
      Parser("aaa . bbb").toString shouldEqual ("((. aaa bbb))")
      Parser("aaa.bbb.ccc").toString shouldEqual ("((. (. aaa bbb) ccc))")
      Parser("a.b[].c").toString shouldEqual ("((. ((. a b)) c))")
      Parser("aaa.bbb[x y.ddd].ccc").toString shouldEqual ("((. ((. aaa bbb) x (. y ddd)) ccc))")
      Parser("a.b[d e][x].f[]").toString shouldEqual ("(((. (((. a b) d e) x) f)))")
      Parser("aaa.b ccc").toString shouldEqual ("((. aaa b) ccc)")
    }

    "Parse call exp" in {
      Parser("a[b c 3 \"str\"]").toString shouldEqual ("""((a b c 3 "str"))""")
      Parser("a[]").toString shouldEqual ("((a))")
      Parser("a[b c][d]").toString shouldEqual ("(((a b c) d))")
    }

    "Parse colon exp" in {
      Parser("a:b").toString shouldEqual ("((: a b))")
      Parser("a:b:c").toString shouldEqual ("((: a (: b c)))")
      Parser("a.b:c").toString shouldEqual ("((: (. a b) c))")
      Parser("a:b.c").toString shouldEqual ("((: a (. b c)))")
      Parser("a:b[]").toString shouldEqual ("((: a (b)))")
      Parser("a[]:b").toString shouldEqual ("((: (a) b))")
      Parser("f[foo [a:int b:array[string]]:type-a]").toString shouldEqual ("((f foo (: (array (: a int) (: b (array string))) type-a)))")
    }

    "Parse MExps" in {
      Parser("""a[]
          |b[]
          |c.d""".stripMargin).toString shouldEqual ("((a) (b) (. c d))")
    }
  }
}
