package koolcNew
package analyzer

import koolcNew.analyzer.Types._
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
    for (cls <- prog.classes) {
      //check if class already defined
      val className = cls.id.value
      val dupClass = gScope.lookupClass(className)
      if (dupClass != None) {
        fatal("Class already defined", cls)
      }
      val cSym = createClsSym(cls)
      cSym.setType(TObject(cSym))
      cls.setSymbol(cSym)
      cls.id.setSymbol(cSym)
      cSym.setPos(cls)
      if (cSym.name == gScope.mainClass.name)
        fatal("Class cannot have the same name as main object", cSym)
      gScope.classes = gScope.classes + (cSym.name -> cSym)
    }

    //Link all classes together. All classes need to exist to set relations between them(or at least it is easier)
    linkParents(prog.classes)

    //Attach symbols to all statements inside method bodies
    for (cls <- prog.classes) {
      for (m <- cls.methods) {
        for (s <- m.stats) {
          matchStatement(s, m.getSymbol)
        }
        matchExpression(m.retExpr, m.getSymbol)
      }
    }

    //Set up main
    for(stat <- prog.main.stats){
      matchStatement(stat, new MethodSymbol("main", prog.main.getSymbol))

    }

    def createClsSym(cls: ClassDecl): ClassSymbol = {
      val clsSym = new ClassSymbol(cls.id.value)
      for (m <- cls.methods) {
        val mSym = createMetSym(m, clsSym)
        m.setSymbol(mSym)
        m.id.setSymbol(mSym)
        mSym.setPos(m)
        clsSym.methods = clsSym.methods + (mSym.name -> mSym)
      }
      for (variable <- cls.vars) {
        val varName = variable.id.value
        //Look for duplicate var definitions
        val dupVar = clsSym.lookupVar(varName)
        if (dupVar != None) {
          error("Duplicate var definition", variable)
        }
        val vSym = new VariableSymbol(varName)

        variable.setSymbol(vSym)
        variable.id.setSymbol(vSym)

        vSym.setPos(variable)


        clsSym.members = clsSym.members + (vSym.name -> vSym)
      }
      clsSym
    }

    def linkParents(classDecls: List[ClassDecl]) = {
      for (t <- classDecls) {
        if (t.parent != None) {
          //Check if parent class is declared
          val clsSym = t.getSymbol
          //Look for inheritance cycles
          var parentSym = gScope.lookupClass(t.parent.get.value).orNull
          if (t.parent.get.value == gScope.mainClass.name) fatal("Main object cannot be inherited", t)
          if (parentSym == null) fatal("Parent class not declared", t)
          clsSym.parent = Some(parentSym)

          /* Look for inheritance cycles, by looping
          * through all parent and see if we ever end up
          * in the same place again, mite b broken*/
          var break = false
          var history = List[ClassSymbol](parentSym)
          while (!break) {
            if (parentSym.parent != None) {
              if (history.contains(parentSym.parent.get)) {
                fatal("Inheritance cycle detected", parentSym)
              } else {
                parentSym = parentSym.parent.orNull
                history = history.::(parentSym)
              }
            } else {
              break = true
            }
          }

        }
      }

      //Look for errors
      for (cls <- classDecls) {
        val clsSym = cls.getSymbol



        for (met <- cls.methods) {
          val metSym = met.getSymbol


          //Check if var type is declared if it is a class
          if (met.args != null) {
            for (v <- met.args) {


              val dupl = v.tpe match {
                case Identifier(name) =>
                  if (name == gScope.mainClass.name) error("Main object cannot be used as type", v)
                  gScope.lookupClass(name).orNull

                case _ => 1
              }
              if (dupl == null) error("Type class not declared", v)

              v.getSymbol.setType(findType(v.tpe))
            }
          }
          if (met.vars != null) {
            for (v <- met.vars) {
              val dupl = v.tpe match {
                case Identifier(name) =>
                  if (name == gScope.mainClass.name) error("Main object cannot be used as type", v)
                  gScope.lookupClass(name).orNull

                case _ => 1
              }

              if (dupl == null) error("Type class not declared", v)

              v.getSymbol.setType(findType(v.tpe))
            }
          }

          if (clsSym.parent != None) {
            //Check if overloaded method declaration exists
            val otherMeth = clsSym.parent.get.lookupMethod(metSym.name).orNull

            if (otherMeth != null) {
              //Check if method fits overloading constraint
              if (metSym.argList.length != otherMeth.argList.length) {
                //Overriding does not apply
                error("Overloading not allowed, must have the same amount of parameters", metSym)
              }
              metSym.argList.zip(otherMeth.argList).filter(x => x._1.getType != x._2.getType)
              .map(x => error("Overriding parameters need to be of the same type. Found: "+x._1.getType+", expected: "+x._2.getType, x._1))

            }
            metSym.overridden = clsSym.parent.get.lookupMethod(metSym.name)
          }

          //Set types after all errors have been caught
          metSym.setType(findType(met.retType))
        }
        for(clsVar <- cls.vars){
          clsVar.getSymbol.setType(findType(clsVar.tpe))
        }
      }

    }

    def findType(tpe: TypeTree) = tpe match {
      case int: IntType =>
        TInt
      case str: StringType =>
        TString
      case bool: BooleanType =>
        TBoolean
      case arr: IntArrayType =>
        TIntArray
      case id: Identifier =>
        val cls = gScope.lookupClass(id.value)
        if(cls != None)
          TObject(cls.get)
        else
          TUntyped
    }

    def createMetSym(met: MethodDecl, clsSym: ClassSymbol): MethodSymbol = {
      val metSym = new MethodSymbol(met.id.value, clsSym)
      metSym.setPos(met.id)

      if (met.args != null) {
        //Methods
        //parameters
        for (p <- met.args) {
          val vSym = new VariableSymbol(p.id.value)
          p.setSymbol(vSym)
          p.id.setSymbol(vSym)
          vSym.setPos(p)

          metSym.params = metSym.params + (vSym.name -> vSym)
          //Used for parameter list comparisons
          metSym.argList = metSym.argList.::(vSym)
        }
      }
      if (met.vars != null) {
        //variables
        for (variable <- met.vars) {
          val varName = variable.id.value
          //Look for duplicate var defs
          val dupVar = metSym.lookupVar(varName).getOrElse[VariableSymbol](metSym.classSymbol.lookupVar(varName).orNull)
          if (dupVar != null) {
            error("Duplicate var definition", variable)
          }
          val vSym = new VariableSymbol(varName)

          variable.setSymbol(vSym)
          variable.id.setSymbol(vSym)

          vSym.setPos(variable)

          metSym.members = metSym.members + (vSym.name -> vSym)
        }
      }
      //Pretty bad, depends heavily on order of execution
      val duplMet = clsSym.lookupMethod(metSym.name) //Parent are not set yet, so this checks only the local class
      if (duplMet != None) {
        error("Duplicate method definition", metSym)
      }

      metSym
    }

    def matchStatement(statement: StatTree, m: MethodSymbol): Boolean = {
      statement match {
        case If(expr, thn, els) =>
          matchExpression(expr, m)
          matchStatement(thn, m)
          if (els != None) matchStatement(els.get, m)
        case While(expr, stat) =>
          matchExpression(expr, m)
          matchStatement(stat, m)
        case Println(expr) =>
          matchExpression(expr, m)
        case Block(stats) =>
          for (stat <- stats)
            matchStatement(stat, m)
        case Assign(id, expr) =>
          val varExists = m.lookupVar(id.value).getOrElse[VariableSymbol](m.classSymbol.lookupVar(id.value).orNull)
          if (varExists != null) {
            id.setSymbol(varExists)


          } else {
            //Variable is never declared
            fatal("Variable not declared", id)
          }

          matchExpression(expr, m)
        case ArrayAssign(_, _, _) =>
          val arrAssStmt = statement.asInstanceOf[ArrayAssign]
          val sym = m.lookupVar(arrAssStmt.id.value).getOrElse[VariableSymbol](m.classSymbol.lookupVar(arrAssStmt.id.value).orNull)
          if (sym != null) arrAssStmt.id.setSymbol(sym)
          else fatal("Array not declared", arrAssStmt.id)
          matchExpression(arrAssStmt.expr, m)
          matchExpression(arrAssStmt.index, m)
      }
      false
    }

    def matchExpression(expr: ExprTree, m: MethodSymbol): Unit = {
      var ret:Symbol = null
      expr match {
        case and: And =>
          and.setType(TBoolean)
          matchExpression(and.lhs, m)
          matchExpression(and.rhs, m)
        case or: Or =>
          or.setType(TBoolean)
          matchExpression(or.lhs, m)
          matchExpression(or.rhs, m)
        case plus: Plus =>
          matchExpression(plus.lhs, m)
          matchExpression(plus.rhs, m)
          if(plus.lhs.getType == TString || plus.rhs.getType == TString)
            plus.setType(TString)
          else
            plus.setType(TInt)
        case minus: Minus=>
          minus.setType(TInt)
          matchExpression(minus.lhs, m)
          matchExpression(minus.rhs, m)
        case times: Times =>
          times.setType(TInt)
          matchExpression(times.lhs, m)
          matchExpression(times.rhs, m)
        case div: Div =>
          div.setType(TInt)
          matchExpression(div.lhs, m)
          matchExpression(div.rhs, m)
        case lt: LessThan =>
          lt.setType(TBoolean)
          matchExpression(lt.lhs, m)
          matchExpression(lt.rhs, m)
        case eq: Equals =>
          eq.setType(TBoolean)
          matchExpression(eq.lhs, m)
          matchExpression(eq.rhs, m)
        case ar: ArrayRead =>
          ar.setType(TInt)
          matchExpression(ar.arr, m)
          matchExpression(ar.index, m)
        case al: ArrayLength =>
          al.setType(TInt)
          matchExpression(al.arr, m)
        case mc: MethodCall =>

          val foo = matchExpression(mc.obj, m)
          val cls = gScope.lookupClass(mc.obj.getType.toString)
          if(cls != None){
            val met = cls.get.lookupMethod(mc.meth.value)
            mc.setType(met.get.getType)
          }else{
            error("Object not defined", mc)
          }

          for (arg <- mc.args) {
            matchExpression(arg, m)
          }


        case n: New =>
          val sym = gScope.lookupClass(n.tpe.value)
          if (sym != None) {
            n.tpe.setSymbol(sym.get)
            n.tpe.setType(TObject(sym.get))
            ret = n.tpe.getSymbol
            n.setType(TObject(sym.get))
          } else { //If the class is not declared we cant instantiate it
            fatal("Class not declared", n.tpe)
          }
        case newArr: NewIntArray =>
          newArr.setType(findType(IntArrayType()))
          matchExpression(newArr.size, m)
        case not: Not =>
          not.setType(TBoolean)
          matchExpression(not.expr, m)
        case thisExpr: This =>
          thisExpr.setSymbol(m.classSymbol)
          thisExpr.setType(TObject(m.classSymbol))
          ret = thisExpr.getSymbol
        case id: Identifier =>
          //Check if variable exists
          val sym = m.lookupVar(id.value).getOrElse[VariableSymbol](m.classSymbol.lookupVar(id.value).orNull)
          if (sym != null) {
            sym.used = true
            id.setSymbol(sym)
            ret = id.getSymbol
          }else{
            error("Variable not declared", id)
          }
        case il: IntLit => il.setType(TInt)
        case sl: StringLit => sl.setType(TString)
        case bt: True => bt.setType(TBoolean)
        case bf: False => bf.setType(TBoolean)
        case _ =>
      }
      ret
    }

    for (cls <- prog.classes) {
      for (vrs <- cls.vars) {
        if (!vrs.getSymbol.used) warning("Variable not used", vrs)
      }
      for (m <- cls.methods) {
        for (vrs <- m.vars) {
          if (!vrs.getSymbol.used) warning("Variable not used", vrs)
        }
      }
    }
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    prog
  }
}
