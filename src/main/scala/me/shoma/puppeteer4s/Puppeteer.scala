package me.shoma.puppeteer4s

class Puppeteer private (val variableName: String, val option: Array) extends Variable(variableName) {

  def launch(args: Seq[String]) = new Puppeteer(variableName, new Array("args", args)) {
    override def compile: String = super.compile + s".launch({${option.compile}})"
  }

}

object Puppeteer {
  def apply(): Puppeteer = new Puppeteer("puppeteer", new Array("args", Seq.empty[String]))
}
