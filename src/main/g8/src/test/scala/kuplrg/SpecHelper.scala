package kuplrg

import scala.quoted.*

def getCode(expr: Expr[Any])(using Quotes): Expr[String] = Expr(norm(expr.show))
transparent inline def codeOf(inline x: Any): String = ${ getCode('x) }

val len = 70
def norm(expr: Any): String =
  val str = expr.toString
    .replaceAll("kuplrg.Implementation.", "")
  if (str.length <= len) str
  else s"${str.substring(0, len)} ..."
