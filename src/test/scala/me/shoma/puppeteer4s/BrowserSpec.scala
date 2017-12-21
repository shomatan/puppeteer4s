package me.shoma.puppeteer4s

import org.scalatest._

class BrowserSpec extends FlatSpec with DiagrammedAssertions {

  private val program = Browser()     // create browser
    .newPage()                        // create page instance
    .goto("https://www.google.com")   // go to google.com
    .screenShot("test.png", true)     // take a screen shot
    .evaluate()                       // get html source
    .toReturn                         // return html source

  private val sourceCode = Function(program).compile

  "Function" should "build programs" in {

    val expectArray =
      """
        |async function run() {
        |  const browser = await puppeteer.launch({args: ['--no-sandbox', '--disable-setuid-sandbox']});
        |  const page = await browser.newPage();
        |  await page.goto('https://www.google.com');
        |  await page.screenshot({path: 'test.png', fullPage: true});
        |  return await page.evaluate(() => document.body.innerHTML);
        |}
      """.stripMargin.split("\n")

    val actual = sourceCode.split("\n")

    assert(actual(0) === expectArray(0))
    assert(actual(1) === expectArray(1))
    assert(actual(2) === expectArray(2))
    assert(actual(3) === expectArray(3))
    assert(actual(4) === expectArray(4))
    assert(actual(5) === expectArray(5))
    assert(actual(6) === expectArray(6))
  }
}
