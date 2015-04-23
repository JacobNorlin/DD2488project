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
    for (cls <- prog.classes) {
      //check if class already defined
      val className = cls.id.value
      val dupClass = gScope.lookupClass(className)
      if (dupClass != None) {
        fatal("Class already defined", cls)
      }
      val cSym = createClsSym(cls)
      cls.setSymbol(cSym)
      cls.id.setSymbol(cSym)
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
        if (dupVar != None) {
          fatal("Duplicate var definition", variable)
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
      for (cls <- classDecls) {
        val clsSym = cls.getSymbol
        for (met <- cls.methods) {
          val metSym = met.getSymbol

          //Check if var type is declared if it is a class
          if (met.args != null) {
            for (v <- met.args) {
              val dupl = v.tpe match {
                case Identifier(name) =>
                  if (name == gScope.mainClass.name) fatal("Main object cannot be used as type", v.getSymbol)
                  gScope.lookupClass(name).orNull

                case _ => 1
              }
              if (dupl == null) fatal("Type class not declared", v)
            }
          }
          if (met.vars != null) {
            for (v <- met.vars) {
              println(v.tpe)
              val dupl = v.tpe match {
                case Identifier(name) =>
                  if (name == gScope.mainClass.name) fatal("Main object cannot be used as type", v)
                  gScope.lookupClass(name).orNull

                case _ => 1
              }

              if (dupl == null) fatal("Type class not declared", v)
            }
          }

          if (clsSym.parent != None) {
            //Check if overloaded method declaration exists
            val otherMeth = clsSym.parent.get.lookupMethod(metSym.name).orNull
            if (otherMeth != null) {
              println(metSym.classSymbol.name, metSym.name, metSym.argList, otherMeth.classSymbol.name, otherMeth.name, otherMeth.argList)
              //Check if method fits overloading constraint
              if (metSym.argList != otherMeth.argList) {
                //Overriding does not apply
                fatal("Overloading not allowed", metSym)
              }
            }
            metSym.overridden = clsSym.parent.get.lookupMethod(metSym.name)
          }
        }
      }

    }

    def createMetSym(met: MethodDecl, clsSym: ClassSymbol): MethodSymbol = {
      val metSym = new MethodSymbol(met.id.value, clsSym)

      if (met.args != null) {
        //Methods
        //parameters
        for (p <- met.args) {
          val vSym = new VariableSymbol(p.id.value)
          p.setSymbol(vSym)
          p.id.setSymbol(vSym)
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
          val dupVar = metSym.lookupVar(varName)
          if (dupVar != None) {
            fatal("Duplicate var definition", variable)
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
        fatal("Method already declared", metSym)
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
          val varExists = m.lookupVar(id.value) //Will also check class, and parents and whatevs

          if (varExists != None) {
            id.setSymbol(varExists.get)
          } else {
            //Variable is never declared
            fatal("Variable not declared", id)
          }

          matchExpression(expr, m)
        case ArrayAssign(_, _, _) =>
          val arrAssStmt = statement.asInstanceOf[ArrayAssign]
          val sym = m.lookupVar(arrAssStmt.id.value)
          if (sym != None) arrAssStmt.id.setSymbol(sym.get)
          else fatal("Array not declared", arrAssStmt.id)
          matchExpression(arrAssStmt.expr, m)
          matchExpression(arrAssStmt.index, m)
      }
      false
    }

    def matchExpression(expr: ExprTree, m: MethodSymbol): Boolean = {
      expr match {
        case And(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Or(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Plus(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Minus(lhs, rhs) =>
          val minusExpr =
            matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Times(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Div(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case LessThan(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case Equals(lhs, rhs) =>
          matchExpression(lhs, m)
          matchExpression(rhs, m)
        case ArrayRead(arr, index) =>
          matchExpression(arr, m)
          matchExpression(index, m)
        case ArrayLength(arr) =>
          matchExpression(arr, m)
        case MethodCall(obj, meth, args) =>
          matchExpression(obj, m)
          for (arg <- args) {
            matchExpression(arg, m)
          }
          meth.setSymbol(new MethodSymbol("???", new ClassSymbol("???")))
        case New(tpe) =>
          val sym = gScope.lookupClass(tpe.value)
          if (sym != None) {
            tpe.setSymbol(sym.get)
          } else { //If the class is not declared
            fatal("Class not declared", tpe)
          }
        case NewIntArray(size) =>
          matchExpression(size, m)
        case Not(expr) =>
          matchExpression(expr, m)
        case This() =>
          val thisExpr = expr.asInstanceOf[This]
          thisExpr.setSymbol(m.classSymbol)
        case Identifier(value) =>
          val id = expr.asInstanceOf[Identifier]
          //Check if variable exists
          val sym = m.lookupVar(id.value)
          if (sym != None) {
            sym.get.used = true
            id.setSymbol(sym.get)
          } else fatal("Variable not declared", id)
        case _ =>
      }
      false
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
