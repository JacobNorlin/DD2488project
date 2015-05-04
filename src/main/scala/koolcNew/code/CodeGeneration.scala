package koolcNew
package code

import ast.Trees._
import analyzer.Symbols._
import koolcNew.analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

import scala.collection.immutable.HashMap

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    var varIndex = new HashMap[Int, Int]()


    def slotFor(index: Int): Int = {
      varIndex.get(index).getOrElse(-1)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val cs = ct.getSymbol
      val classFile = new ClassFile(cs.name, cs.parent.map(_.name))
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor
      //Add fields
      for (v <- ct.vars) {
        val varSym = v.getSymbol
        val field = classFile.addField(convertType(varSym.getType), varSym.name)
      }
      //Add methods
      for (m <- ct.methods) {
        val metSym = m.getSymbol
        val method = classFile.addMethod(convertType(metSym.getType), metSym.name, m.args.map(x => convertType(x.getSymbol.getType)))
        println(metSym.name)
        generateMethodCode(method.codeHandler, m)
      }

      classFile.writeToFile(dir + "/" + sourceName + ".class")
    }

    /*Method for converting our types to CafeBabe types*/
    def convertType(t: Type): String = {
      t match {
        case TInt =>
          "I"
        case TString =>
          "Ljava/lang/String;"
        case TBoolean =>
          "Z"
        case TIntArray =>
          "[I"
        case TObject(clsSym) =>
          "L"+clsSym.name + ";" //Probably wrong
      }
    }


    def compileStatement(ch: CodeHandler, st: StatTree): CodeHandler = {
      st match {
        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          compileExpression(ch, expr)
          //println(expr, convertType(expr.getType))
          ch << InvokeVirtual("java/io/PrintStream", "println", "("+convertType(expr.getType)+")V") <<
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
          for (stat <- stats){
            compileStatement(ch, stat)
          }
          null

        case ArrayAssign(id, index, expr) =>
          compileExpression(ch, expr)
          compileExpression(ch, index)
          ch << ALoad(slotFor(id.getSymbol.id))
          ch << IASTORE

        case Assign(id, expr) =>
          val sym = id.getSymbol
          compileExpression(ch, expr)
          expr.getType match {
            case TInt => ch << IStore(slotFor(sym.id))
            case TString => ch << AStore(slotFor(sym.id))
            case TBoolean => ch << IStore(slotFor(sym.id))
            case TObject(clsSym) => ch << AStore(slotFor(sym.id))
            case TIntArray => ch << AStore(slotFor(sym.id))
          }

      }
    }
    def compileExpression(ch: CodeHandler, e: ExprTree): CodeHandler = {
      e match {
        case And(lhs, rhs) =>
          val nExit = ch.getFreshLabel("nExit");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          ch << IfEq(nExit)
          compileExpression(ch, rhs)
          ch << Goto(nTrue)
          ch << Label(nExit)
          ch << ICONST_0
          ch << Label(nTrue)

        case Or(lhs, rhs) =>
          val nExit = ch.getFreshLabel("nExit");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          ch << IfEq(nExit)
          ch << ICONST_1
          ch << Goto(nTrue)
          ch << Label(nExit)
          compileExpression(ch, rhs)
          ch << Label(nTrue)
        case Plus(lhs, rhs) if e.getType == TString =>
          ch << DefaultNew("java/lang/StringBuilder")
          compileExpression(ch, lhs)
          ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
          compileExpression(ch, rhs)
          ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
          ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
        case Plus(lhs,rhs) =>
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          ch << IADD
        case Minus(lhs, rhs) =>1
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
          val nExit = ch.getFreshLabel("nExit");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          ch << If_ICmpLt(nTrue)
          ch << ICONST_0
          ch << Goto(nExit)
          ch << Label(nTrue)
          ch << ICONST_1
          ch << Label(nExit)

        case Equals(lhs, rhs) =>
          val nExit = ch.getFreshLabel("nExit");
          val nTrue = ch.getFreshLabel("nTrue")
          compileExpression(ch, lhs)
          compileExpression(ch, rhs)
          lhs.getType match {
            case TInt | TBoolean => ch << If_ICmpEq(nTrue)
            case _ => ch << If_ACmpEq(nTrue)
          }
          ch << ICONST_0
          ch << Goto(nExit)
          ch << Label(nTrue)
          ch << ICONST_1
          ch << Label(nExit)
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
          val args = mc.args.map(arg => {
            compileExpression(ch, arg)
            convertType(arg.getType)
          })

          var argConcat = "("
          for (arg <- args) {
            argConcat = argConcat + arg
          }
          argConcat = argConcat + ")"
          argConcat = argConcat + convertType(mc.getType)
          ch << InvokeVirtual(mc.obj.getType.toString, mc.meth.value, argConcat)
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
            case TInt | TBoolean => ch << ILoad(slotFor(id.getSymbol.id))
            case TString | TObject(_) | TIntArray => ch << ALoad(slotFor(id.getSymbol.id))
          }
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case True() =>
          ch << ICONST_1
        case False() =>
          ch << ICONST_0


      }

    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      var index = 1

      for(arg <- mt.args){
        varIndex += Tuple2(arg.getSymbol.id, index)
        index = index + 1
      }

      for(v <- mt.vars){
        varIndex += Tuple2(v.getSymbol.id, ch.getFreshVar)
      }

      for (stat <- mt.stats) {
        compileStatement(ch, stat)
      }
      compileExpression(ch, mt.retExpr)
      mt.retExpr.getType match {
        case TInt | TBoolean => ch << IRETURN
        case TObject(_) | TString | TIntArray => ch << ARETURN
        case _ => ch << RETURN
      }

      ch.print


      // TODO: Emit code

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      for (stat <- stmts) {
        println(stat)
        compileStatement(ch, stat)
      }
      // TODO: Emit code
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }
    val sourceName = ctx.file.getName


    // output code
    prog.classes foreach {
      ct => generateClassFile(ct.id.value, ct, outDir)
    }

    val mainClass = new ClassFile(prog.main.id.value)

    mainClass.setSourceFile(sourceName)
    mainClass.addDefaultConstructor
    val ch: CodeHandler = mainClass.addMainMethod.codeHandler
    generateMainMethodCode(ch, prog.main.stats, prog.main.id.value)
    mainClass.writeToFile(prog.main.id.value + ".class")
    // Now do the main method
    // ...



  }

}
