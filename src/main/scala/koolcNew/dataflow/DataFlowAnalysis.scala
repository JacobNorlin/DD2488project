package koolcNew.dataflow

import koolcNew.ast.Trees.{ExprTree, VarDecl}
import koolcNew.dataflow.Domain.{Top, Bottom, nullDereference, State}
import koolcNew.dataflow.Graph
import koolcNew.dataflow.Graph.ControlFlowGraph
import koolcNew.dataflow.Nodes.ArrayAssign
import koolcNew.dataflow.Nodes.Assign
import koolcNew.dataflow.Nodes._
import koolcNew.ast.Trees._
import scala.collection.mutable

/**
 * Created by jacob on 2015-06-03.
 */
object DataFlowAnalysis {

  def run(graph: ControlFlowGraph) = {

    val in = mutable.Map[Node, mutable.Map[Int, State]]()
    var out = mutable.Map[Node, mutable.Map[Int, State]]()


    for((id, node) <- graph.nodes){
      var nodeMap = mutable.Map[Int, State]()
      for(v <- graph.vars){
        nodeMap = nodeMap + (v.getSymbol.id -> Bottom())
      }
      in(node) = nodeMap
      out = in
    }

    for(((cls, met), metGraph) <- graph.methodMap){
      val metMap = mutable.Map[Int, State]()
      for(clsVar <- cls.vars){
        metMap(clsVar.getSymbol.id) = Top()
      }
      for(metVar <- met.vars){
        metMap(metVar.getSymbol.id) = Top()
      }
      in(metGraph.nodes.head._2) = metMap
    }


    def forwardAnalysis(transfer: (Node, mutable.Map[Int, State]) => mutable.Map[Int, State]): Unit = {
      var unchanged = false

      while(!unchanged){
        for((id, node) <- graph){
          val tm = transfer(node, in(node))
          unchanged = tm == out(node)
          out(node) = tm
        }
      }

    }

    def uninitError(nodeMap: mutable.Map[Int, State], id: Identifier) ={
      nodeMap(id.getSymbol.id) match {
        case Top() =>
          println(s"Variable $id may be uninitialized")
        case Bottom() =>
      }
    }

    def errorCheck(nodeId: Option[Int]): Int = {
      nodeId match{
        case Some(n) => {
          val node = graph.nodes.get(n).get
          val nodeMap = in(node)
          node match{
            case as: Assign =>
              val ids = parseExpression(as.expr)

              for(id <- ids){
                uninitError(nodeMap, id)
              }
              errorCheck(as.next)
            case ar: ArrayAssign =>
              val ids = parseExpression(ar.index) ++ parseExpression(ar.expr)
              for(id <- ids) {
                //Could be generalized to higher order function specified in domain
                uninitError(nodeMap, id)
              }
              errorCheck(ar.next)
            case ex: Expression =>
              val ids = parseExpression(ex.expr)
              for(id <- ids) {
                uninitError(nodeMap, id)
              }
              errorCheck(ex.next)
            case br: Branch =>
              for(brNode <- br.next)
                errorCheck(Some(brNode))
            case mr: Merge =>
              errorCheck(mr.next)
            case cfn: ControlFlowNode =>
              errorCheck(cfn.next)
            case ret: Return =>
              errorCheck(ret.next)

          }
        }

      }
      0
    }
    
    def parseExpression(expr: ExprTree): List[Identifier] = {
      expr match {
        case And(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case Or(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case Plus(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case Minus(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case Times(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case Div(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case LessThan(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case Equals(lhs: ExprTree, rhs: ExprTree) => parseExpression(lhs) ++ parseExpression(rhs)
        case ArrayRead(arr: ExprTree, index: ExprTree) => parseExpression(arr) ++ parseExpression(index)
        case ArrayLength(arr: ExprTree) => parseExpression(arr)
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => args.flatMap(arg => parseExpression(arg))
        case id: Identifier =>
          List(id)
          


        case Not(expr: ExprTree) => parseExpression(expr)
        case _ => List()
      }
    }


    val dom1 = nullDereference()

    forwardAnalysis(dom1.transfer)

    errorCheck(Some(graph.start.get.idNum))




  }

}
