package example.nested

trait LocalDeclarations/*example.nested.LocalDeclarations#*/:
  def foo(): Unit

trait Foo/*example.nested.Foo#*/:
  val y = 3

object LocalDeclarations/*example.nested.LocalDeclarations.*/:
  def create(): LocalDeclarations =
    def bar(): Unit = ()

    val x = new:
      val x = 2

    val y = new Foo {}

    val yy = y.y

    new LocalDeclarations with Foo:
      override def foo(): Unit = bar()
