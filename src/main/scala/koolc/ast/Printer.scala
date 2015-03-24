package koolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    var sb:StringBuilder = new StringBuilder()

    val main = t.asInstanceOf[Program].main

    sb.append("object "+main.id+" { \n")
    sb.append("\t def main() : Unit = { \n")
    printStatements(main.stats)
    sb.append("}\n")
    sb.append("}")


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
          sb.append(assignNode.id.value+" = ")
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
          sb.append(arrayNode.id.value+"[")
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
        printExpression(expression)
        sb.append("\n")
      }

    }

    def printExpression(expression:ExprTree):String = {
      expression.getClass match {
        case Plus => {
          val plusNode = expression.asInstanceOf[Plus]
          sb.append("(")
          sb.append(printExpression(plusNode.lhs))
          sb.append(" + ")
          sb.append(printExpression(plusNode.rhs))
          sb.append(")")
          ""
        }
        case Minus => {
          val minusNode = expression.asInstanceOf[Minus]
          sb.append("(")
          sb.append(printExpression(minusNode.lhs))
          sb.append(" - ")
          sb.append(printExpression(minusNode.rhs))
          sb.append(")")
          ""
        }
        case Times => {
          val timesNode = expression.asInstanceOf[Times]
          sb.append("(")
          sb.append(printExpression(timesNode.lhs))
          sb.append(" * ")
          sb.append(printExpression(timesNode.rhs))
          sb.append(")")
          ""
        }
        case Div => {
          val divNode = expression.asInstanceOf[Div]
          sb.append("(")
          sb.append(printExpression(divNode.lhs))
          sb.append(" * ")
          sb.append(printExpression(divNode.rhs))
          sb.append(")")
          ""
        }
        case And => {
          val andNode = expression.asInstanceOf[And]
          sb.append("(")
          sb.append(printExpression(andNode.lhs))
          sb.append(" && ")
          sb.append(printExpression(andNode.rhs))
          sb.append(")")
          ""
        }
        case Or => {
          val orNode = expression.asInstanceOf[Or]
          sb.append("(")
          sb.append(printExpression(orNode.lhs))
          sb.append("||")
          sb.append(printExpression(orNode.rhs))
          sb.append(")")
          ""
        }
        case LessThan => {
          val ltNode = expression.asInstanceOf[LessThan]
          sb.append("(")
          sb.append(printExpression(ltNode.lhs))
          sb.append(" < ")
          sb.append(printExpression(ltNode.rhs))
          sb.append(")")
          ""
        }
        case Equals => {
          val equalsNode = expression.asInstanceOf[Equals]
          sb.append("(")
          sb.append(printExpression(equalsNode.lhs))
          sb.append(" == ")
          sb.append(printExpression(equalsNode.rhs))
          sb.append(")")
          ""
        }
        case ArrayRead => {
          val arNode = expression.asInstanceOf[ArrayRead]
          printExpression(arNode.arr)
          sb.append("[")
          printExpression(arNode.index)
          sb.append("]")
          ""
        }
        case ArrayLength => {
          val alNode = expression.asInstanceOf[ArrayLength]
          printExpression(alNode.arr)
          sb.append(".length")
          ""
        }
        case MethodCall => {
          val mcNode = expression.asInstanceOf[MethodCall]
          printExpression(mcNode.obj)
          sb.append("."+mcNode.meth.value+"( ")
          printExpressions(mcNode.args)
          sb.append(" );")
          ""
        }
        case IntLit => {
          val intNode = expression.asInstanceOf[IntLit]
          sb.append(intNode.value
          ""
        }
        case StringLit => {
          val strNode = expression.asInstanceOf[StringLit]
          sb.append(strNode.value)
          ""
        }
        case True => {
          sb.append("true")
          ""
        }
        case False => {
          sb.append("false")
          ""
        }
        case Identifier => {
          val idNode = expression.asInstanceOf[Identifier]
          sb.append(idNode.value)
          ""
        }
        case This => {
          sb.append("this")
          ""
        }
        case NewIntArray => {
          val niaNode = expression.asInstanceOf[NewIntArray]
          sb.append("new int[")
          printExpression(niaNode.size)
          sb.append("]")
          ""
        }
        case New =>{
          val newNode = expression.asInstanceOf[New]
          sb.append("new "+newNode.tpe.value+"()")
          ""
        }
        case Not => {
          val notNode = expression.asInstanceOf[Not]
          sb.append("!")
          printExpression(notNode.expr)
          ""
        }

      }

    }

    def printType(nodeType: TypeTree) = nodeType.getClass match {
      case IntType => {
        sb.append("Int")
      }
      case BooleanType => {
        sb.append("Bool")
      }
      case StringType => {
        sb.append("String")
      }
      case IntArrayType => {
        sb.append("Int[]")
      }
      case Identifier => {
        val idNode = nodeType.asInstanceOf[Identifier]
        sb.append(idNode.value)
      }
    }


    sb.toString()
  }
}
