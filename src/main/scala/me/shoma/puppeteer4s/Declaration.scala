package me.shoma.puppeteer4s

abstract class Declaration(declare: String) extends Expr {
  override def compile: String = declare
}

case object Const extends Declaration("const")
case object Await extends Declaration("await")
