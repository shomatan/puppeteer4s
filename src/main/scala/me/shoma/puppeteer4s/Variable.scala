package me.shoma.puppeteer4s

case class Variable(name: String) extends Expr {
  override def compile: String = name
}
