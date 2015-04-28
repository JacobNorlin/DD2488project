import koolcNew.ast.Trees._

import scala.Equals

/**
 * Created by jacob on 2015-04-28.
 */
object testUtils {

  def getAllNames(prog: Program): List[String]= {
    def getArgs(met: MethodDecl): List[String] = {
      if (met.args != null) {
        met.args.map(arg => arg.getSymbol.name)
      } else {
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.name) ++
        cls.vars.map(v => v.getSymbol.name) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.name) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.name)
        })
    }).sortBy(x => x)
  }

  def getAllNames(prog: koolc.ast.Trees.Program): List[String]= {
    def getArgs(met: koolc.ast.Trees.MethodDecl): List[String] = {
      if (met.args != null) {
        met.args.map(arg => arg.getSymbol.name)
      } else {
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.name) ++
        cls.vars.map(v => v.getSymbol.name) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.name) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.name)
        })
    }).sortBy(x => x)
  }


  def getAllIds(prog: Program): List[Int]= {
    def getArgs(met:MethodDecl):List[Int] = {
      if(met.args != null){
        met.args.map(arg => arg.getSymbol.id)
      }else{
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.id) ++
        cls.vars.map(v => v.getSymbol.id) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.id) ++
            getExprIds(met.retExpr) ++
            met.stats.flatMap(stat => getStatementIds(stat)) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.id)
        })
    }).sortBy(x => x)

  }

  def getStatementIds(t:StatTree):List[Int] = {

    val r = t match {
      case If(expr, thn, els) =>
        val ret:List[Int] = List()
        ret++getExprIds(expr)++
          getStatementIds(thn)
        if(els != None && els != null) ret++getStatementIds(els.get)
        ret
      case While(expr, stat) =>
        getStatementIds(stat) ++
          getExprIds(expr)
      case Block(stats) =>
        stats.flatMap(stat => getStatementIds(stat)) //Shoold return a list of ints...
      case Println(expr) =>
        getExprIds(expr)
      case ArrayAssign(id, index, expr) =>
        List(id.getSymbol.id) ++
          getExprIds(index)++
          getExprIds(expr)
      case Assign(id, expr) =>
        getExprIds(expr)++
          List(id.getSymbol.id)
    }
    r
  }

  def getExprIds(e: ExprTree):List[Int] = {
    val r:List[Int] = e match {
      case And(lhs, rhs) =>
        getExprIds(lhs) ++
          getExprIds(rhs)
      case Or(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case Plus(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case Minus(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case Times(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case Div(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case LessThan(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case Equals(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case ArrayRead(arr, index) =>
        getExprIds(arr)++
          getExprIds(index)
      case ArrayLength(arr) =>
        getExprIds(arr)
      case mc: MethodCall =>
        val ret = List()
        ret++getExprIds(mc.obj)

        for (arg <- mc.args) {
          ret++getExprIds(arg)
        }
        ret
      case New(tpe) =>
        List(tpe.getSymbol.id)
      case newArr: NewIntArray =>
        getExprIds(newArr.size)
      case Not(expr) =>
        getExprIds(expr)
      case thisExpr: This =>
        List(thisExpr.getSymbol.id)

      case id: Identifier =>
        List(id.getSymbol.id)
      case _ =>
        List()
    }
    r
  }

  def getAllIds(prog: koolc.ast.Trees.Program): List[Int] = {
    def getArgs(met: koolc.ast.Trees.MethodDecl): List[Int] = {
      if (met.args != null) {
        met.args.map(arg => arg.getSymbol.id)
      } else {
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.id) ++
        cls.vars.map(v => v.getSymbol.id) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.id) ++
            getExprIds(met.retExpr) ++
            met.stats.flatMap(stat => getStatementIds(stat)) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.id)
        })
    }).sortBy(x => x)

  }

  def getStatementIds(t:koolc.ast.Trees.StatTree):List[Int] = {

    val r = t match {
      case koolc.ast.Trees.If(expr, thn, els) =>
        val ret:List[Int] = List()
        ret++getExprIds(expr)++
          getStatementIds(thn)
        if(els != None && els != null) ret++getStatementIds(els.get)
        ret
      case koolc.ast.Trees.While(expr, stat) =>
        getStatementIds(stat) ++
          getExprIds(expr)
      case koolc.ast.Trees.Block(stats) =>
        stats.flatMap(stat => getStatementIds(stat)) //Shoold return a list of ints...
      case koolc.ast.Trees.Println(expr) =>
        getExprIds(expr)
      case koolc.ast.Trees.ArrayAssign(id, index, expr) =>
        List(id.getSymbol.id) ++
          getExprIds(index)++
          getExprIds(expr)
      case koolc.ast.Trees.Assign(id, expr) =>
        getExprIds(expr)++
          List(id.getSymbol.id)
    }
    r
  }

  def getExprIds(e: koolc.ast.Trees.ExprTree):List[Int] = {
    val r:List[Int] = e match {
      case koolc.ast.Trees.And(lhs, rhs) =>
        getExprIds(lhs) ++
          getExprIds(rhs)
      case koolc.ast.Trees.Or(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.Plus(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.Minus(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.Times(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.Div(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.LessThan(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.Equals(lhs, rhs) =>
        getExprIds(lhs)++
          getExprIds(rhs)
      case koolc.ast.Trees.ArrayRead(arr, index) =>
        getExprIds(arr)++
          getExprIds(index)
      case koolc.ast.Trees.ArrayLength(arr) =>
        getExprIds(arr)
      case mc: koolc.ast.Trees.MethodCall =>
        val ret = List()
        ret++getExprIds(mc.obj)

        for (arg <- mc.args) {
          ret++getExprIds(arg)
        }
        ret
      case koolc.ast.Trees.New(tpe) =>
        List(tpe.getSymbol.id)
      case newArr: koolc.ast.Trees.NewIntArray =>
        getExprIds(newArr.size)
      case koolc.ast.Trees.Not(expr) =>
        getExprIds(expr)
      case thisExpr: koolc.ast.Trees.This =>
        List(thisExpr.getSymbol.id)

      case id: koolc.ast.Trees.Identifier =>
        List(id.getSymbol.id)
      case _ =>
        List()
    }
    r
  }

}
