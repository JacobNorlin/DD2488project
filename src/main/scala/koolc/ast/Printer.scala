package koolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    var sb:StringBuilder = new StringBuilder()

    val main = t.asInstanceOf[Program].main

    sb.append("object "+main.id+" { \n")
    sb.append("\t def main() : Unit = { \n")


    def printStatements(statements:List[StatTree]) = {
      for(statement <- statements){
          printStatement(statement)
      }
    }

    def printStatement(statement:StatTree):String = {
      statement.getClass match {
        case While => {
          val whileNode = statement.asInstanceOf[While]
          sb.append("while( ")
          printExpression(whileNode.expr)
          sb.append(" )\n")
          printStatement(whileNode.stat)

        }

        case If => {
          val ifNode = statement.asInstanceOf[If]
          sb.append("if( ")
          printExpression(ifNode.expr)
          sb.append(" ) ")
          printStatement(ifNode.thn)
          if(ifNode.els != null){
            sb.append("else ")
            sb.append("\t")
            printStatement(ifNode.els.get)
          }
          ""
        }

        case Assign => {
          val assignNode = statement.asInstanceOf[Assign]
          sb.append(assignNode.id+" = ")
          printExpression(assignNode.expr)
          sb.append(";\n")
          ""
        }

        case Println => {
          val printNode = statement.asInstanceOf[Println]
          sb.append("println( ")
          printExpression(printNode.expr)
          sb.append(" );\n")
          ""
        }

        case ArrayAssign => {
          val arrayNode = statement.asInstanceOf[ArrayAssign]
          sb.append(arrayNode.id+"[")
          printExpression(arrayNode.index)
          sb.append("] = ")
          printExpression(arrayNode.expr)
          sb.append(";\n")
          ""
        }

        case Block => {
          val blockNode = statement.asInstanceOf[Block]
          sb.append("{\n")
          printStatements(blockNode.stats)
          sb.append("}\n")
          ""
        }
      }

    }

    def printExpressions(expressions:List[ExprTree]) = {

      for(expression <- expressions){

      }

    }

    def printExpression(expression:ExprTree):String = {
      expression.getClass match {
        case Plus => {
          var plusNode = expression.asInstanceOf[Plus]
          sb.append(printExpression(plusNode.lhs))
          sb.append(" + ")
          sb.append(printExpression(plusNode.rhs))
          ""
        }
      }

    }

  }
}
