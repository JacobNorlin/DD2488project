package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    for(cls <- prog.classes){
      for(m <- cls.methods){
        for(stat <- m.stats){
          tcStat(stat)
        }
        tcExpr(m.retExpr, m.getSymbol.getType)
      }
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        case Plus(lhs, rhs) =>
          val lhsType = tcExpr(lhs, TString, TInt)
          val rhsType = tcExpr(rhs, TString, TInt)
          if(lhsType == TString || rhsType == TString)
            TString
          else
            TInt
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBoolean
        case Equals(lhs, rhs) =>
          val lhsType = tcExpr(lhs, TInt, TString, TBoolean, anyObject)
          val rhsType = tcExpr(rhs, TInt, TString, TBoolean, anyObject)
          if(!lhsType.isSubTypeOf(anyObject) || !rhsType.isSubTypeOf(anyObject)){
            if(lhsType != rhsType)
              error("Invalid comparison between: " + lhsType + " and " + rhsType, lhs)
          }
          TBoolean
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          TInt
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
          TInt
        case met: MethodCall =>
          tcExpr(met.obj, tcExpr(met.obj))
          for (arg <- met.args) {
            tcExpr(arg, TInt, TString, TBoolean, TIntArray, anyObject)
          }
          //println(met.getType)
          met.getType
        case New(tpe) =>
          tcExpr(tpe, tpe.getType)
          TObject(tpe.getSymbol.asInstanceOf[ClassSymbol])
        case newArr: NewIntArray =>
          tcExpr(newArr.size, TInt)
          TIntArray
        case Not(expr) =>
          tcExpr(expr, TBoolean)
          TBoolean
        case thisExpr: This =>
          thisExpr.getType
        case id: Identifier =>
          id.getType
        case IntLit(_) =>
          TInt
        case StringLit(_) =>
          TString
        case True() =>
          TBoolean
        case False() =>
          TBoolean


      } // TODO: Compute type for each kind of expression
     // println(expr, tpe)
      expr.setType(tpe)
      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case If(expr, thn, els) =>
          tcExpr(expr,  TBoolean)
          tcStat(thn)
          if(els != None)
            tcStat(els.get)
        case While(expr, stat) =>
          tcExpr(expr, TBoolean)
          tcStat(stat)
        case Println(expr) =>
          tcExpr(expr, TBoolean, TInt, TString)
        case Assign(id, expr) =>
          tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) =>
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
          tcExpr(id, TIntArray)
        case Block(stats) =>
          for(stat <- stats)
            tcStat(stat)
      }
    }

    prog
  }

}
