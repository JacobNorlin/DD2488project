package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // Step 1: Collect symbols in declarations

    var gScope = new GlobalScope

    gScope.mainClass = new ClassSymbol(prog.main.id.value)

    prog.classes.map(cls => {
      val cSym = createClsSym(cls)
      cls.setSymbol(cSym)
      gScope.classes + ("#" + cSym.id -> cSym)
    })

    linkParents(prog.classes)

    def createClsSym(cls: ClassDecl): ClassSymbol = {
      var clsSym = new ClassSymbol(cls.id.value)
      for (m <- cls.methods) {
        val mSym = createMetSym(m, clsSym)
        m.setSymbol(mSym)
        clsSym.methods + ("#" + mSym.id -> mSym)
      }
      for (v <- cls.vars) {
        val vSym = new VariableSymbol(v.id.value)
        v.setSymbol(vSym)
        clsSym.members + ("#" + vSym.id -> vSym)
      }
      clsSym
    }

    def linkParents(classDecls: List[ClassDecl]) = {
      for (t <- classDecls) {
        if (t.parent != null) {
          t.getSymbol.parent = Option(t.parent.get.getSymbol.asInstanceOf[ClassSymbol])
        }
      }
    }

    def createMetSym(met: MethodDecl, clsSym: ClassSymbol): MethodSymbol = {
      var metSym = new MethodSymbol(met.id.value, clsSym)
      for (p <- met.args) {
        val vSym = new VariableSymbol(p.id.value)
        p.setSymbol(vSym)
        metSym.params + ("#" + vSym.id -> vSym)
      }
      for (m <- met.args) {
        val vSym = new VariableSymbol(m.id.value)
        m.setSymbol(vSym)
        metSym.members + ("#" + vSym.id -> vSym)
      }
      //arglist??
      if (clsSym.parent != null) {
        metSym.overridden = clsSym.parent.get.lookupMethod(met.id.value)
      }
      metSym
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    
    
    
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    prog
  }
}
