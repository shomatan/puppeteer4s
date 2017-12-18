package me.shoma.puppeteer4s

sealed abstract class Operator(op: String) extends Expr {
  override def compile: String = op
}

case object EqualOp extends Operator(" = ")
case object NoneOp extends Operator(" ")