package me.shoma.puppeteer4s


trait Line {
  def lines: Seq[Line]
  def compile: String
}

case class Browser() extends Line {
  def newPage(): Page = new Page(this)
  override def lines: Seq[Line] = Seq(this)
  override def compile: String = "const browser = await puppeteer.launch();"
}

class ReturnSyntax(line: Line) extends Line {
  override def lines = line.lines.init :+ this :+ line.lines.last
  override def compile = "return"
}

class Page(browser: Browser) extends Line {

  lazy val self = new Page(this.browser)

  def goto(url: String): Page = {
    new Page(this.browser) {
      override def lines: Seq[Line] = self.lines :+ this
      override def compile: String = s"await page.goto('$url');"
    }
  }

  def evaluate(): Page = {
    new Page(this.browser) {
      override def lines: Seq[Line] = self.lines :+ this
      override def compile: String = "await page.evaluate(() => document.body.innerHTML);"
    }
  }

  def toReturn: ReturnSyntax = {
    new ReturnSyntax(this)
  }

  override def lines: Seq[Line] = browser.lines :+ this
  override def compile = "const page = await browser.newPage();"
}

case class Function(line: Line) extends Line {
  override def lines = line.lines
  override def compile: String =
    s"""
      |async function run() {
      |  ${line.lines.map(_.compile).mkString("\n  ")}
      |}
    """.stripMargin
}

object App extends App {
  val program = Browser()             // create browser
    .newPage()                        // create page instance
    .goto("https://www.google.com")   // go to google.com
    .evaluate()                       // get html source
    .toReturn                         // return html source

  val sourceCode = Function(program).compile

  println(sourceCode)
}