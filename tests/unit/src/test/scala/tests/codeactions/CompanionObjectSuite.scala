package tests.codeactions

import scala.meta.internal.metals.codeactions.CreateCompanionObjectCodeAction

class CompanionObjectSuite extends BaseCodeActionLspSuite("companionObject") {

  check(
    "insert-companion-object",
    """|class F<<>>oo {
       |  
       |}
       |
       |class Bar {
       |
       |}
       |""".stripMargin,
    s"""|${CreateCompanionObjectCodeAction.companionObjectCreation}""".stripMargin,
    """|class Foo {
       |  
       |}
       |
       |object Foo {
       |
       |}
       |
       |class Bar {
       |
       |}
       |""".stripMargin
  )

}
