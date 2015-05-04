package koolcNew
package code

import ast.Trees._
import analyzer.Symbols._
import koolcNew.analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      var classFile: ClassFile = null
      if(ct.parent.orNull != null)
        classFile = new ClassFile(ct.getSymbol.name, Some(ct.parent.get.getSymbol.name))
      else
        classFile = new ClassFile(ct.getSymbol.name, None)
      //Add fields
      for(v <- ct.vars){
        val varSym = v.getSymbol
        val field = classFile.addField(convertType(varSym.getType), varSym.name)
      }
      //Add methods
      for(m <- ct.methods){
        val metSym = m.getSymbol
        val method = classFile.addMethod(convertType(metSym.getType), metSym.name, m.args.map(x => convertType(x.getSymbol.getType)))
        generateMethodCode(method.codeHandler, m)
      }
      classFile.writeToFile(dir+"/"+sourceName)
    }

    /*Method for converting our types to CafeBabe types*/
    def convertType(t: Type):String = {
      t match {
        case TInt =>
          "I"
        case TString =>
          "LJava\\lang\\String;"
        case TBoolean =>
          "Z"
        case TIntArray =>
          "[I"
        case TObject(clsSym) =>
          "./"+clsSym.name //Probably wrong
      }
    }

    def compileStatement(ch: CodeHandler, st: StatTree): CodeHandler = {
      st match {
        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          compileExpression(ch, expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
            RETURN
        case If(expr, thn, els) =>
          val nElse = ch.getFreshLabel("nElse");
          val nAfter = ch.getFreshLabel("nAfter")
          compileExpression(ch, expr)
          ch << IfEq(nElse)
          compileStatement(ch, thn)
          ch << Goto(nAfter)
          ch << Label(nElse)
          compileStatement(ch, els.orNull)
          ch << Label(nAfter)


        case While(expr, stat) =>
          val nStart = ch.getFreshLabel("nStart");
          val nExit = ch.getFreshLabel("nExit")
          ch << Label(nStart)
          compileExpression(ch, expr)
          ch << IfEq(nExit)
          compileStatement(ch, stat)
          ch << Goto(nStart)
          ch << Label(nExit)

        case Block(stats) =>
          for (stat <- stats)
            compileStatement(ch, stat)
          null

        case ArrayAssign(id, index, expr) =>
          compileExpression(ch, expr)
          compileExpression(ch, index)
          ch << ALoad(id.getSymbol.id)
          ch << IASTORE

        case Assign(id, expr) =>
          val sym = id.getSymbol
          compileExpression(ch, expr)
          expr.getType match {
            case TInt => ch << IStore(sym.id)
            case TString => ch << AStore(sym.id)
            case TBoolean => ch << IStore(sym.id)
            case TObject(clsSym) => ch << AStore(sym.id)
          }
        case _ =>
          ch << ICONST_0

      }
    }
    def compileExpression(ch: CodeHandler, e: ExprTree): CodeHandler = {
      e match {
        case And(lhs, rhs) =>
          val nFalse = ch.getFreshLabel("nFalse");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          ch << IfEq(nFalse)
          compileExpression(ch, rhs)
          ch << Goto(nTrue)
          ch << Label(nFalse)
          ch << ICONST_0
          ch << Label(nTrue)

        case Or(lhs, rhs) =>
          val nFalse = ch.getFreshLabel("nFalse");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          ch << IfEq(nFalse)
          ch << ICONST_1
          ch << Goto(nTrue)
          ch << Label(nFalse)
          compileExpression(ch, rhs)
          ch << Label(nTrue)
        case Plus(lhs, rhs) =>
          if (lhs.getType == TInt && rhs.getType == TInt) {
            compileExpression(ch, lhs)
            compileExpression(ch, rhs)
            ch << IADD
          } else {
            ch << DefaultNew("java/lang/StringBuilder")
            compileExpression(ch, lhs)
            compileExpression(ch, rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "Ljava/lang/String;Ljava/lang/String;V")
          }
        case Minus(lhs, rhs) =>
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          ch << ISUB
        case Times(lhs, rhs) =>
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          ch << IMUL
        case Div(lhs, rhs) =>
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          ch << IDIV
        case LessThan(lhs, rhs) =>
          val nFalse = ch.getFreshLabel("nFalse");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          ch << If_ICmpLt(nTrue)
          ch << ICONST_0
          ch << Goto(nFalse)
          ch << Label(nTrue)
          ch << ICONST_1
          ch << Label(nFalse)

        case Equals(lhs, rhs) =>
          val nFalse = ch.getFreshLabel("nFalse");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          lhs.getType match {
            case TInt | TBoolean => ch << If_ICmpEq(nTrue)
            case _ => ch << If_ACmpEq(nTrue)
          }
          ch << ICONST_0
          ch << Goto(nFalse)
          ch << Label(nTrue)
          ch << ICONST_1
          ch << Label(nFalse)
        case ArrayRead(arr, index) =>
          compileExpression(ch, arr)
          ch << ALOAD
          compileExpression(ch, index)
          ch << LDC
        case ArrayLength(arr) =>
          compileExpression(ch, arr)
          ch << ARRAYLENGTH
        case mc: MethodCall =>
          compileExpression(ch, mc.obj)
          mc.args.map(arg => compileExpression(ch, arg))
          ch.print
          ch << INVOKEVIRTUAL <<
            (mc.getType match {
              case TInt | TBoolean => IRETURN
              case _ => ARETURN
            })
        case New(tpe) =>
          ch << DefaultNew(tpe.value)
        case newArr: NewIntArray =>
          compileExpression(ch, newArr.size)
          ch << NEWARRAY
        case Not(expr) =>
          ch << ICONST_1
          compileExpression(ch, expr)
          ch << ISUB
        case thisExpr: This =>
          ch << ALOAD_0
        case id: Identifier =>
          id.getType match {
            case TInt | TBoolean => ch << ILoad(id.getSymbol.id)
            case TString | TObject(_) | TIntArray => ch << ALoad(id.getSymbol.id)
          }
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case True() =>
          ch << ICONST_1
        case False() =>
          ch << ICONST_0
        case _ =>
          ch << ICONST_0
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      mt.vars.zipWithIndex foreach {case  (v, i) =>
          v.id.getType match {
            case TInt | TBoolean => ch << IStore(v.id.getSymbol.id)
            case _ => ch << IStore(v.id.getSymbol.id)
          }
      }

      mt.args.zipWithIndex foreach {case (arg,i) =>
        arg.id.getType match {
          case TInt | TBoolean => ch << ILoad(i) << IStore(arg.id.getSymbol.id)
          case _ => ch << ALoad(i) << IStore(arg.id.getSymbol.id)
        }
      }
      for (stat <- mt.stats) {
        compileStatement(ch, stat)
      }
      compileExpression(ch, mt.retExpr)

      // TODO: Emit code

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      for(stat <- stmts){
        compileStatement(ch, stat)
      }
      // TODO: Emit code
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    val mainClass = new ClassFile("main")
    val ch: CodeHandler = mainClass.addMainMethod.codeHandler
    generateMainMethodCode(ch, prog.main.stats, "main")
    mainClass.writeToFile("main")
    // Now do the main method
    // ...
  }

}
