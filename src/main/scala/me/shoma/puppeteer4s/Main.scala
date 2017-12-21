package me.shoma.puppeteer4s

trait Expr {
  def compile: String
}

case class Sentence(leftExpr: LeftExpr, operator: Operator, rightExpr: RightExpr) extends Expr {
  override def compile: String = s"${leftExpr.compile}${operator.compile}${rightExpr.compile};"
}

case class LeftExpr(declaration: Declaration, variable: Variable) extends Expr {
  override def compile: String = s"${declaration.compile} ${variable.compile}"
}

case class RightExpr(declaration: Declaration, process: String) extends Expr {
  override def compile: String = s"${declaration.compile} $process"
}

trait Line {
  def lines: Seq[Line]
  def compile: String
}

class Array(val arrayName: String, val values: Seq[String]) extends Variable(arrayName) {

  private val argsToString = values.map(s => s"'$s'").mkString(", ")

  override def compile: String = super.compile + s": [$argsToString]"
}

case class Browser() extends Line {

  private val leftExpr  = LeftExpr(Const, Variable("browser"))
  private val operator  = EqualOp
  private val puppeteer = Puppeteer().launch(Seq("--no-sandbox", "--disable-setuid-sandbox"))
  private val rightExpr = RightExpr(Await, puppeteer.compile)

  def newPage(): Page = new Page(this, lines)

  override def lines = Seq(this)
  override def compile: String = Sentence(leftExpr, operator, rightExpr).compile
}

class ReturnSyntax(line: Line) extends Line {
  override def lines: Seq[Line] = line.lines.init :+ this
  override def compile: String = "return " + line.compile
}

class Page(browser: Browser, list: Seq[Line]) extends Line {

  def goto(url: String): Page = new Page(browser, list :+ this) {
    override def compile: String = s"await page.goto('$url');"
  }

  def evaluate(): Page = new Page(browser, list :+ this) {
    override def compile: String = "await page.evaluate(() => document.body.innerHTML);"
  }

  def screenShot(path: String, fullPage: Boolean): Page = new Page(browser, list :+ this) {
    override def compile: String = s"await page.screenshot({path: '$path', fullPage: $fullPage});"
  }

  def toReturn: ReturnSyntax = new ReturnSyntax(this)

  override def lines: Seq[Line] = list :+ this
  override def compile = "const page = await browser.newPage();"
}

case class Function(line: Line) extends Line {
  override def lines = line.lines
  override def compile: String =
    s"""
      |async function run() {
      |  ${lines.map(_.compile).mkString("\n  ")}
      |}
    """.
      stripMargin
}

object App extends App {
  val program = Browser()             // create browser
    .newPage()                        // create page instance
    .goto("https://www.google.com")   // go to google.com
    .screenShot("test.png", true)     // take a screen shot
    .evaluate()                       // get html source
    .toReturn                         // return html source

  val sourceCode = Function(program).compile

  println(sourceCode)

  /* Results

  async function run() {
    const browser = await puppeteer.launch({args: ['--no-sandbox', '--disable-setuid-sandbox']});
    const page = await browser.newPage();
    await page.goto('https://www.google.com');
    await page.screenshot({path: 'test.png', fullPage: true});
    return await page.evaluate(() => document.body.innerHTML);
  }

   */
}
