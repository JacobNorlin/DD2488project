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

    //Create class symbols
    for(cls <- prog.classes){
      //check if class already defined
      val className = cls.id.value
      val dupClass = gScope.lookupClass(className)
      if(dupClass != None){
        fatal("Class already defined", cls)
      }
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
      for (variable <- cls.vars) {
        val varName = variable.id.value
        //Look for duplicate var definitions
        val dupVar = clsSym.lookupVar(varName)
        if(dupVar != None){
          fatal("Duplicate var definition", variable)
        }
        val vSym = new VariableSymbol(varName)
        variable.setSymbol(vSym)
        variable.id.setSymbol(vSym)
        vSym.setPos(variable)


        clsSym.members = clsSym.members + (vSym.name-> vSym)
      }
      clsSym
    }

    def linkParents(classDecls: List[ClassDecl]) = {
      for (t <- classDecls) {
        if (t.parent != null) {
          if(gScope.lookupClass(t.parent.get.value) == null) fatal("parent class not declared", t)
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
        //Methods
        for (p <- met.args) {
          val vSym = new VariableSymbol(p.id.value)
          p.setSymbol(vSym)
          p.id.setSymbol(vSym)
          metSym.params = metSym.params + (vSym.name -> vSym)
        }
      }
      if(met.vars != null){
        //variables
        for (variable <- met.vars) {
          val varName = variable.id.value
          //Look for duplicate var defs
          val dupVar = metSym.lookupVar(varName)
          if(dupVar != None){
            fatal("Duplicate var definition",variable)
          }
          val vSym = new VariableSymbol(varName)
          variable.setSymbol(vSym)
          variable.id.setSymbol(vSym)
          vSym.setPos(variable)

          metSym.members = metSym.members + (vSym.name -> vSym)
        }
      }
      //arglist??

      metSym
    }
    //Attach symbols to all statements inside method bodies
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
        case If(expr,thn,els) =>
          matchExpression(expr, m)
          matchStatement(thn, m)
          if(els != None) matchStatement(els.get, m)
        case While(expr, stat) =>
          matchExpression(expr, m)
          matchStatement(stat, m)
        case Println(expr) =>
          matchExpression(expr, m)
        case Block(stats) =>
          for (stat <- stats)
            matchStatement(stat, m)
        case Assign(id,expr) =>
          val v = m.lookupVar(id.value)
          if(v != None) id.setSymbol(v.get)
          else fatal("variable not declared", id)
          matchExpression(expr, m)
        case ArrayAssign(_,_,_) => val arrAssStmt = statement.asInstanceOf[ArrayAssign]
          val sym = m.lookupVar(arrAssStmt.id.value)
          if(sym != None) arrAssStmt.id.setSymbol(sym.get)
          else fatal("array not declared", arrAssStmt.id)
          matchExpression(arrAssStmt.expr, m)
          matchExpression(arrAssStmt.index, m)
      }
      false
    }

    def matchExpression(expr: ExprTree, m:MethodSymbol): Boolean = {
      expr match {
        case And(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Or(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Plus(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Minus(lhs,rhs) => val minusExpr =
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Times(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Div(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case LessThan(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Equals(lhs,rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case ArrayRead(arr,index) =>
          matchExpression(arr, m)
          matchExpression(index, m)
        case ArrayLength(arr) =>
          matchExpression(arr, m)
        case MethodCall(obj,meth,args) =>
          matchExpression(obj, m)
          for (arg <- args) {
            println(arg)
            matchExpression(arg, m)
          }
          meth.setSymbol(new MethodSymbol("???", new ClassSymbol("???")))
        case New(tpe) =>
          val sym = gScope.lookupClass(tpe.value)
          if(sym != None)
            tpe.setSymbol(sym.get)
        case NewIntArray(size) =>
          matchExpression(size, m)
        case Not(expr) =>
          matchExpression(expr, m)
        case This() => val thisExpr = expr.asInstanceOf[This]
          thisExpr.setSymbol(m.classSymbol)
        case Identifier(value) => val id = expr.asInstanceOf[Identifier]
          val sym = m.lookupVar(id.value)
          if(sym != None){
            id.setSymbol(sym.get)
          } else fatal("variable not declared", id)
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
