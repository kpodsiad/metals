package tests.tokens

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams

import munit.Location
import munit.TestOptions
import tests.BasePCSuite
import tests.TestSemanticTokens

class SemanticTokensSuite extends BasePCSuite {

  override protected def ignoreScalaVersion: Option[IgnoreScalaVersion] = Some(
    IgnoreScala3
  )
  check(
    "class, object, var, val(readonly), method, type, parameter, String(single-line)",
    s"""|<<class>>/*keyword*/  <<Test>>/*class*/{
        |
        | <<var>>/*keyword*/ <<wkStr>>/*variable*/ = <<"Dog-">>/*string*/
        | <<val>>/*keyword*/ <<nameStr>>/*variable,readonly*/ = <<"Jack">>/*string*/
        |
        | <<def>>/*keyword*/ <<Main>>/*method*/={
        |
        |  <<val>>/*keyword*/ <<preStr>>/*variable,readonly*/= <<"I am ">>/*string*/
        |  <<var>>/*keyword*/ <<postStr>>/*variable*/= <<"in a house. ">>/*string*/
        |  <<wkStr>>/*variable*/=<<nameStr>>/*variable,readonly*/ <<+>>/*method*/ <<"Cat-">>/*string*/
        |
        |  <<testC>>/*class*/.<<bc>>/*method*/(<<preStr>>/*variable,readonly*/
        |    <<+>>/*method*/ <<wkStr>>/*variable*/
        |    <<+>>/*method*/ <<preStr>>/*variable,readonly*/)
        | }
        |}
        |
        |<<object>>/*keyword*/  <<testC>>/*class*/{
        |
        | <<def>>/*keyword*/ <<bc>>/*method*/(<<msg>>/*parameter*/:<<String>>/*type*/)={
        |   <<println>>/*method*/(<<msg>>/*parameter*/)
        | }
        |}
        |""".stripMargin,
  )

  check(
    "Comment(Single-Line, Multi-Line)",
    s"""|
        |<<object>>/*keyword*/ <<Main>>/*class*/{
        |
        |   <</**>>/*comment*/
        |<<   * Test of Comment Block>>/*comment*/
        |<<   */>>/*comment*/  <<val>>/*keyword*/ <<x>>/*variable,readonly*/ = <<1>>/*number*/
        |
        |  <<def>>/*keyword*/ <<add>>/*method*/(<<a>>/*parameter*/ : <<Int>>/*class,abstract*/) = {
        |    <<// Single Line Comment>>/*comment*/
        |    <<a>>/*parameter*/ <<+>>/*method,abstract*/ <<1>>/*number*/ <<// com = 1>>/*comment*/
        |   }
        |}
        |
        |
        |""".stripMargin,
  )

  check(
    "number literal, Static",
    s"""|
        |<<object>>/*keyword*/ <<ab>>/*class*/ {
        |  <<var>>/*keyword*/  <<iVar>>/*variable*/:<<Int>>/*class,abstract*/ = <<1>>/*number*/
        |  <<val>>/*keyword*/  <<iVal>>/*variable,readonly*/:<<Double>>/*class,abstract*/ = <<4.94065645841246544e-324d>>/*number*/
        |  <<val>>/*keyword*/  <<fVal>>/*variable,readonly*/:<<Float>>/*class,abstract*/ = <<1.40129846432481707e-45>>/*number*/
        |  <<val>>/*keyword*/  <<lVal>>/*variable,readonly*/:<<Long>>/*class,abstract*/ = <<9223372036854775807L>>/*number*/
        |}
        |
        |<<object>>/*keyword*/ <<sample10>>/*class*/ {
        |  <<def>>/*keyword*/ <<main>>/*method*/(<<args>>/*parameter*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
        |    <<println>>/*method*/(
        |     (<<ab>>/*class*/.<<iVar>>/*variable*/ <<+>>/*method,abstract*/ <<ab>>/*class*/.<<iVal>>/*variable,readonly*/).<<toString>>/*method*/
        |    )
        |  }
        |}
        |""".stripMargin,
  )

  check(
    "abstract(modifier), trait, type parameter",
    s"""|
        |<<package>>/*keyword*/ <<a>>/*namespace*/.<<b>>/*namespace*/
        |<<object>>/*keyword*/ <<Sample5>>/*class*/ {
        |
        |  <<def>>/*keyword*/ <<main>>/*method*/(<<args>>/*parameter*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
        |      <<val>>/*keyword*/ <<itr>>/*variable,readonly*/ = <<new>>/*keyword*/ <<IntIterator>>/*class*/(<<5>>/*number*/)
        |      <<var>>/*keyword*/ <<str>>/*variable*/ = <<itr>>/*variable,readonly*/.<<next>>/*method*/().<<toString>>/*method*/ <<+>>/*method*/ <<",">>/*string*/
        |          <<str>>/*variable*/ += <<itr>>/*variable,readonly*/.<<next>>/*method*/().<<toString>>/*method*/
        |      <<println>>/*method*/(<<"count:">>/*string*/<<+>>/*method*/<<str>>/*variable*/)
        |  }
        |
        |  <<trait>>/*keyword*/ <<Iterator>>/*interface,abstract*/[<<A>>/*typeParameter,abstract*/] {
        |    <<def>>/*keyword*/ <<next>>/*method,abstract*/(): <<A>>/*typeParameter,abstract*/
        |  }
        |
        |  <<abstract>>/*modifier*/ <<class>>/*keyword*/ <<hasLogger>>/*class,abstract*/ {
        |    <<def>>/*keyword*/ <<log>>/*method*/(<<str>>/*parameter*/:<<String>>/*type*/) = {<<println>>/*method*/(<<str>>/*parameter*/)}
        |  }
        |
        |  <<class>>/*keyword*/ <<IntIterator>>/*class*/(<<to>>/*variable,readonly*/: <<Int>>/*class,abstract*/)
        |  <<extends>>/*keyword*/ <<hasLogger>>/*class,abstract*/ <<with>>/*keyword*/ <<Iterator>>/*interface,abstract*/[<<Int>>/*class,abstract*/]  {
        |    <<private>>/*modifier*/ <<var>>/*keyword*/ <<current>>/*variable*/ = <<0>>/*number*/
        |    <<override>>/*modifier*/ <<def>>/*keyword*/ <<next>>/*method*/(): <<Int>>/*class,abstract*/ = {
        |      <<if>>/*keyword*/ (<<current>>/*variable*/ <<<>>/*method,abstract*/ <<to>>/*variable,readonly*/) {
        |        <<log>>/*method*/(<<"main">>/*string*/)
        |        <<val>>/*keyword*/ <<t>>/*variable,readonly*/ = <<current>>/*variable*/
        |        <<current>>/*variable*/ = <<current>>/*variable*/ <<+>>/*method,abstract*/ <<1>>/*number*/
        |        <<t>>/*variable,readonly*/
        |      } <<else>>/*keyword*/ <<0>>/*number*/
        |    }
        |  }
        |}
        |
        |
        |""".stripMargin,
  )

  check(
    "deprecated",
    s"""|<<object>>/*keyword*/ <<sample9>>/*class*/ {
        |  <<@>>/*keyword*/<<deprecated>>/*class*/(<<"this method will be removed">>/*string*/, <<"FooLib 12.0">>/*string*/)
        |  <<def>>/*keyword*/ <<oldMethod>>/*method,deprecated*/(<<x>>/*parameter*/: <<Int>>/*class,abstract*/) = <<x>>/*parameter*/
        |
        |  <<def>>/*keyword*/ <<main>>/*method*/(<<args>>/*parameter*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
        |    <<val>>/*keyword*/ <<str>>/*variable,readonly*/ = <<oldMethod>>/*method,deprecated*/(<<2>>/*number*/).<<toString>>/*method*/
        |     <<println>>/*method*/(<<"Hello, world!">>/*string*/<<+>>/*method*/ <<str>>/*variable,readonly*/)
        |  }
        |}
        |""".stripMargin,
  )

  check(
    "import(Out of File)",
    s"""|
        |<<import>>/*keyword*/ <<scala>>/*namespace*/.<<math>>/*namespace*/.<<sqrt>>/*method*/
        |<<object>>/*keyword*/ <<sample3>>/*class*/ {
        |
        |  <<def>>/*keyword*/ <<sqrtplus1>>/*method*/(<<x>>/*parameter*/: <<Int>>/*class,abstract*/)
        |     = <<sqrt>>/*method*/(<<x>>/*parameter*/).<<toString>>/*method*/()
        |
        |  <<def>>/*keyword*/ <<main>>/*method*/(<<args>>/*parameter*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
        |    <<println>>/*method*/(<<"Hello, world! : ">>/*string*/ <<+>>/*method*/ <<sqrtplus1>>/*method*/(<<2>>/*number*/))
        |  }
        |}
        |
        |""".stripMargin,
  )

  def check(
      name: TestOptions,
      expected: String,
  )(implicit location: Location): Unit =
    test(name) {

      val base =
        expected
          .replaceAll(raw"/\*[\w,]+\*/", "")
          .replaceAll(raw"\<\<|\>\>", "")

      val tokens = presentationCompiler
        .semanticTokens(
          CompilerVirtualFileParams(URI.create("file:/Tokens.scala"), base)
        )
        .get()

      val obtained = TestSemanticTokens.semanticString(
        base,
        tokens.asScala.toList.map(_.toInt),
      )
      assertEquals(
        obtained,
        expected,
      )

    }
}
