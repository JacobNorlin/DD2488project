package koolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    var sb:StringBuilder = new StringBuilder()

    val main = t.asInstanceOf[Program].main

    var tabCounter = 0


    sb.append("object "+main.id.value)
    startBlock
    putTabs
    sb.append("def main() : Unit =")
    startBlock
    printStatements(main.stats)
    endBlock
    endBlock

    printClasses(t.asInstanceOf[Program].classes)
    
    def putTabs = {
      val tabs = "\t"*tabCounter
      sb.append(tabs)
    }

    def startBlock = {
      sb.append(" {\n")
      tabCounter += 1

    }
    def endBlock = {
      tabCounter -= 1
      putTabs
      sb.append("}\n")
    }

    def printClasses(classDecls: List[ClassDecl]) = {
      classDecls.map(classDecl => {
        printClass(classDecl)
        putTabs
      })
    }

    def printClass(classDecl: ClassDecl) = {
      sb.append("class ")
      sb.append(classDecl.id.value)
      if(classDecl.parent != null){
        sb.append(" extends ")
        sb.append(classDecl.parent.get.value)
      }
      startBlock
      printVars(classDecl.vars)
      printMethods(classDecl.methods)
      endBlock
    }

    def printMethods(methodDecls: List[MethodDecl]) = {
      methodDecls.map(method => {
        putTabs
        printMethod(method)
      })
    }

    def parseType(tpe:TypeTree): String = tpe match {
      case IntType() => "Int"
      case StringType() => "String"
      case BooleanType() => "Boolean"
      case IntArrayType() => "Int[]"
      case Identifier(_) => tpe.asInstanceOf[Identifier].value
      case _ => "NoSuchType"
    }

    def printMethod(method: MethodDecl): Unit ={
      sb.append("def ")
      sb.append(method.id.value)
      sb.append(" (")
      if(method.args != null){
        sb.append(method.args.head.id.value+" : "+parseType(method.args.head.tpe))
        method.args.tail.map(x => {
          sb.append(", "+x.id.value + " : " + parseType(x.tpe))
        })
      }
      sb.append(") : ")
      sb.append(parseType(method.retType)+ " = ")
      startBlock
      printVars(method.vars)
      printStatements(method.stats)
      putTabs
      sb.append("return ")
      printExpression(method.retExpr)
      sb.append(";\n")
      endBlock
    }

    def printVars(varDecls: List[VarDecl]) = {
      varDecls.map(varDecl => {
        putTabs
        printVar(varDecl)
      })
    }

    def printVar(varDecl: VarDecl) = {
      sb.append("var ")
      sb.append(varDecl.id.value)
      sb.append(" : ")
      sb.append(parseType(varDecl.tpe))
      sb.append(";\n")
    }


    def printStatements(statements:List[StatTree]) = {
      statements.map(statement => {
        putTabs
        printStatement(statement)
      })
    }

    def printStatement(statement:StatTree):String = {
      statement match {
        case While(_,_) => {
          val whileNode = statement.asInstanceOf[While]
          sb.append("while ( ")
          printExpression(whileNode.expr)
          sb.append(" ) ")
          printStatement(whileNode.stat)
          ""
        }

        case If(_,_,_) => {
          val ifNode = statement.asInstanceOf[If]
          sb.append("if ( ")
          printExpression(ifNode.expr)
          sb.append(" ) ")
          printStatement(ifNode.thn)
          if(ifNode.els != null){
            putTabs
            sb.append("else ")
            printStatement(ifNode.els.get)
          }
          ""
        }

        case Assign(_,_) => {
          val assignNode = statement.asInstanceOf[Assign]
          sb.append(assignNode.id.value+" = ")
          printExpression(assignNode.expr)
          sb.append(";\n")
          ""
        }

        case Println(_) => {
          val printNode = statement.asInstanceOf[Println]
          sb.append("println( ")
          printExpression(printNode.expr)
          sb.append(" );\n")
          ""
        }

        case ArrayAssign(_,_,_) => {
          val arrayNode = statement.asInstanceOf[ArrayAssign]
          sb.append(arrayNode.id.value+"[")
          printExpression(arrayNode.index)
          sb.append("] = ")
          printExpression(arrayNode.expr)
          sb.append(";\n")
          ""
        }

        case Block(_) => {
          val blockNode = statement.asInstanceOf[Block]
          startBlock
          printStatements(blockNode.stats)
          endBlock
          ""
        }
      }

    }

    def printMethodCallArguments(args:List[ExprTree]) = {
      if(args.length != 0){
        printExpression(args.head)
        if(args.length != 1){
          for(arg <- args.tail){
            sb.append(", ")
            printExpression(arg)
          }
        }


      }


    }



    def printExpression(expression:ExprTree):String = {
      expression match {
        case Plus(_,_) => {
          val plusNode = expression.asInstanceOf[Plus]
          sb.append("(")
          sb.append(printExpression(plusNode.lhs))
          sb.append(" + ")
          sb.append(printExpression(plusNode.rhs))
          sb.append(")")
          ""
        }
        case Minus(_,_) => {
          val minusNode = expression.asInstanceOf[Minus]
          sb.append("(")
          sb.append(printExpression(minusNode.lhs))
          sb.append(" - ")
          sb.append(printExpression(minusNode.rhs))
          sb.append(")")
          ""
        }
        case Times(_,_) => {
          val timesNode = expression.asInstanceOf[Times]
          sb.append("(")
          sb.append(printExpression(timesNode.lhs))
          sb.append(" * ")
          sb.append(printExpression(timesNode.rhs))
          sb.append(")")
          ""
        }
        case Div(_,_) => {
          val divNode = expression.asInstanceOf[Div]
          sb.append("(")
          sb.append(printExpression(divNode.lhs))
          sb.append(" / ")
          sb.append(printExpression(divNode.rhs))
          sb.append(")")
          ""
        }
        case And(_,_) => {
          val andNode = expression.asInstanceOf[And]
          sb.append("(")
          sb.append(printExpression(andNode.lhs))
          sb.append(" && ")
          sb.append(printExpression(andNode.rhs))
          sb.append(")")
          ""
        }
        case Or(_,_) => {
          val orNode = expression.asInstanceOf[Or]
          sb.append("(")
          sb.append(printExpression(orNode.lhs))
          sb.append("||")
          sb.append(printExpression(orNode.rhs))
          sb.append(")")
          ""
        }
        case LessThan(_,_) => {
          val ltNode = expression.asInstanceOf[LessThan]
          sb.append("(")
          sb.append(printExpression(ltNode.lhs))
          sb.append(" < ")
          sb.append(printExpression(ltNode.rhs))
          sb.append(")")
          ""
        }
        case Equals(_,_) => {
          val equalsNode = expression.asInstanceOf[Equals]
          sb.append("(")
          sb.append(printExpression(equalsNode.lhs))
          sb.append(" == ")
          sb.append(printExpression(equalsNode.rhs))
          sb.append(")")
          ""
        }
        case ArrayRead(_,_) => {
          val arNode = expression.asInstanceOf[ArrayRead]
          printExpression(arNode.arr)
          sb.append("[")
          printExpression(arNode.index)
          sb.append("]")
          ""
        }
        case ArrayLength(_) => {
          val alNode = expression.asInstanceOf[ArrayLength]
          printExpression(alNode.arr)
          sb.append(".length")
          ""
        }
        case MethodCall(_,_,_) => {
          val mcNode = expression.asInstanceOf[MethodCall]
          printExpression(mcNode.obj)
          sb.append("."+mcNode.meth.value+"( ")
          printMethodCallArguments(mcNode.args)
          sb.append(" )")
          ""
        }
        case IntLit(_) => {
          val intNode = expression.asInstanceOf[IntLit]
          sb.append(intNode.value)
          ""
        }
        case StringLit(_) => {
          val strNode = expression.asInstanceOf[StringLit]
          sb.append("\""+strNode.value+"\"")
          ""
        }
        case True() => {
          sb.append("true")
          ""
        }
        case False() => {
          sb.append("false")
          ""
        }
        case Identifier(_) => {
          val idNode = expression.asInstanceOf[Identifier]
            sb.append(idNode.value)
          ""
        }
        case This() => {
          sb.append("this")
          ""
        }
        case NewIntArray(_) => {
          val niaNode = expression.asInstanceOf[NewIntArray]
          sb.append("new Int[")
          printExpression(niaNode.size)
          sb.append("]")
          ""
        }
        case New(_) =>{
          val newNode = expression.asInstanceOf[New]
          sb.append("new "+parseType(newNode.tpe)+"()")
          ""
        }
        case Not(_) => {
          val notNode = expression.asInstanceOf[Not]
          sb.append("!")
          printExpression(notNode.expr)
          ""
        }

      }

    }

    def printType(nodeType: TypeTree) = nodeType match {
      case IntType() => {
        sb.append("Int")
      }
      case BooleanType() => {
        sb.append("Bool")
      }
      case StringType() => {
        sb.append("String")
      }
      case IntArrayType() => {
        sb.append("Int[]")
      }
      case Identifier(_) => {
        val idNode = nodeType.asInstanceOf[Identifier]
        sb.append(idNode.value)
      }
    }


    sb.toString()
  }
}
