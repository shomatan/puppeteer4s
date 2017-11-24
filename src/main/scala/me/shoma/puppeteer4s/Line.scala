package me.shoma.puppeteer4s


trait Line {
  def lines: Seq[Line]
  def compile: String
}

case class Browser() extends Line {
  def newPage(): Page = new Page(this, lines)

  override def lines = Seq(this)
  override def compile: String = "const browser = await puppeteer.launch();"
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
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
    await page.goto('https://www.google.com');
    await page.screenshot({path: 'test.png', fullPage: true});
    return await page.evaluate(() => document.body.innerHTML);
  }

   */
}