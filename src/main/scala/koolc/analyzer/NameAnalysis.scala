package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // Step 1: Collect symbols in declarations

    val gScope = new GlobalScope

    gScope.mainClass = new ClassSymbol(prog.main.id.value)
    prog.main.setSymbol(gScope.mainClass)
    prog.main.id.setSymbol(gScope.mainClass)


    for(cls <- prog.classes){
      val cSym = createClsSym(cls)
      cls.setSymbol(cSym)
      cls.id.setSymbol(cSym)

      gScope.classes = gScope.classes + (cSym.name -> cSym)
    }

    linkParents(prog.classes)

    def createClsSym(cls: ClassDecl): ClassSymbol = {
      val clsSym = new ClassSymbol(cls.id.value)
      for (m <- cls.methods) {
        val mSym = createMetSym(m, clsSym)
        m.setSymbol(mSym)
        m.id.setSymbol(mSym)
        clsSym.methods = clsSym.methods + (mSym.name -> mSym)
      }
      for (v <- cls.vars) {
        val vSym = new VariableSymbol(v.id.value)
        v.setSymbol(vSym)
        v.id.setSymbol(vSym)


        clsSym.members = clsSym.members + (vSym.name-> vSym)
      }
      clsSym
    }

    def linkParents(classDecls: List[ClassDecl]) = {
      for (t <- classDecls) {
        if (t.parent != null) {
          t.getSymbol.parent = Some(t.parent.get.getSymbol.asInstanceOf[ClassSymbol])
        }
      }
      for(cls <- classDecls){
        for(m <- cls.methods){

          if (cls.parent != null) {
            m.getSymbol.overridden = cls.getSymbol.parent.asInstanceOf[ClassSymbol].lookupMethod(m.id.value)
          }
        }
      }

    }



    def createMetSym(met: MethodDecl, clsSym: ClassSymbol): MethodSymbol = {
      val metSym = new MethodSymbol(met.id.value, clsSym)
      if(met.args != null) {
        for (p <- met.args) {
          val vSym = new VariableSymbol(p.id.value)
          p.setSymbol(vSym)
          p.id.setSymbol(vSym)
          metSym.params = metSym.params + (vSym.name -> vSym)
        }
      }
      if(met.vars != null){
        for (v <- met.vars) {
          val vSym = new VariableSymbol(v.id.value)
          v.setSymbol(vSym)
          v.id.setSymbol(vSym)
          
          metSym.members = metSym.members + (vSym.name -> vSym)
        }
      }
      //arglist??

      metSym
    }

    for(cls <- prog.classes){
      for(m <- cls.methods){
        for(s <- m.stats){
          matchStatement(s, m.getSymbol)
          }
        matchExpression(m.retExpr, m.getSymbol)
        }
      }
    def matchStatement(statement: StatTree, m: MethodSymbol): Boolean = {
      statement match {
        case If(_,_,_) => val ifStmt = statement.asInstanceOf[If]
          matchExpression(ifStmt.expr, m)
          matchStatement(ifStmt.thn, m)
        case While(_,_) => val whileStmt = statement.asInstanceOf[While]
          matchExpression(whileStmt.expr, m)
          matchStatement(whileStmt.stat, m)
        case Println(_) => val printStmt = statement.asInstanceOf[Println]
          matchExpression(printStmt.expr, m)
        case Block(stats) =>
          for (stat <- stats)
            matchStatement(stat, m)
        case Assign(id,expr) =>
          val v = m.lookupVar(id.value)
          if(v != None) id.setSymbol(v.get)
          matchExpression(expr, m)
        case ArrayAssign(_,_,_) => val arrAssStmt = statement.asInstanceOf[ArrayAssign]
          val sym = m.lookupVar(arrAssStmt.id.value)
          if(sym != None) arrAssStmt.id.setSymbol(sym.get)
          matchExpression(arrAssStmt.expr, m)
          matchExpression(arrAssStmt.index, m)
      }
      false
    }

    def matchExpression(expr: ExprTree, m:MethodSymbol): Boolean = {
      expr match {
        case And(_,_) => val andExpr = expr.asInstanceOf[And]
          matchExpression(andExpr.lhs, m)
          matchExpression(andExpr.rhs, m)
        case Or(_,_) => val orExpr = expr.asInstanceOf[Or]
          matchExpression(orExpr.lhs, m)
          matchExpression(orExpr.rhs, m)
        case Plus(_,_) => val mlusExpr = expr.asInstanceOf[Plus]
          matchExpression(mlusExpr.lhs, m)
          matchExpression(mlusExpr.rhs, m)
        case Minus(_,_) => val minusExpr = expr.asInstanceOf[Minus]
          matchExpression(minusExpr.lhs, m)
          matchExpression(minusExpr.rhs, m)
        case Times(_,_) => val timesExpr = expr.asInstanceOf[Times]
          matchExpression(timesExpr.lhs, m)
          matchExpression(timesExpr.rhs, m)
        case Div(_,_) => val divExpr = expr.asInstanceOf[Div]
          matchExpression(divExpr.lhs, m)
          matchExpression(divExpr.rhs, m)
        case LessThan(_,_) => val ltExpr = expr.asInstanceOf[LessThan]
          matchExpression(ltExpr.lhs, m)
          matchExpression(ltExpr.rhs, m)
        case Equals(_,_) => val eqExpr = expr.asInstanceOf[Equals]
          matchExpression(eqExpr.lhs, m)
          matchExpression(eqExpr.rhs, m)
        case ArrayRead(_,_) => val arrReadExpr = expr.asInstanceOf[ArrayRead]
          matchExpression(arrReadExpr.arr, m)
          matchExpression(arrReadExpr.index, m)
        case ArrayLength(_) => val arrLengthExpr = expr.asInstanceOf[ArrayLength]
          matchExpression(arrLengthExpr.arr, m)
        case MethodCall(obj,meth,args) =>
          println("====="+obj )
          matchExpression(obj, m)
          for (arg <- args) {
            println(arg)
            matchExpression(arg, m)
          }
          meth.setSymbol(new MethodSymbol(("???"), new ClassSymbol("???")))
        case New(tpe) =>
          val sym = gScope.lookupClass(tpe.value)
          if(sym != None)
            tpe.setSymbol(sym.get)
        case NewIntArray(_) => val newIntArrExpr = expr.asInstanceOf[NewIntArray]
          matchExpression(newIntArrExpr.size, m)
        case Not(_) => val notExpr = expr.asInstanceOf[Not]
          matchExpression(notExpr.expr, m)
        case This() => val thisExpr = expr.asInstanceOf[This]
          thisExpr.setSymbol(m.classSymbol)
        case Identifier(_) => val id = expr.asInstanceOf[Identifier]
          val sym = m.lookupVar(id.value)

          if(sym != None){

            id.setSymbol(sym.get)
          }

        case _ => println(expr)
      }
      false
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    
    
    
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    prog
  }
}
