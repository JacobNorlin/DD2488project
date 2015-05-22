package koolcNew.dataflow

import koolcNew.ast.Trees._
import koolcNew.dataflow.Edges._
import koolcNew.dataflow.Nodes.Node

object Graph {

  case class ControlFlowGraph(
                             start: Option[Node],
                             end: Option[Node],
                             nodes: List[Node],
                             edges: List[Edge]
  ){
    def +(node: Node) = addNode(this, node)
    def ::(cfg: ControlFlowGraph) = append(this, cfg)
  }

  val program: Program

  def emptyGraph() = {
    ControlFlowGraph(None, None, List(), List())
  }

  def addNode(cfg: ControlFlowGraph, node: Node):ControlFlowGraph = {
    ControlFlowGraph(cfg.start, cfg.end, cfg.nodes ++ List(node), cfg.edges)
  }

  def append(cfg1: ControlFlowGraph, cfg2: ControlFlowGraph) = {
    ControlFlowGraph(cfg1.start, cfg2.end, cfg1.nodes ++ cfg2.nodes, cfg1.edges ++ cfg2.edges)
  }

  val startNode = Nodes.ControlFlowNode()
  val endNode = Nodes.ControlFlowNode()
  val startEdge = Edge(startNode, endNode)
  var graph = ControlFlowGraph(Some(startNode), Some(endNode), List(startNode, endNode), List(startEdge))

  var previousNode = startNode



  def buildGraph(prog: Program): Unit ={



  }


  def decomposeStat(stat: StatTree, previousNode: Node): ControlFlowGraph = {
    stat match {
      case Assign(id, expr) =>
        val assignNode = Nodes.Assign(id, expr)
        ControlFlowGraph(Some(previousNode),
          Some(assignNode), List(previousNode, assignNode),
          List(Edge(previousNode, assignNode)))
      case ArrayAssign(id, index, expr) =>
        val arrAssignNode = Nodes.ArrayAssign(id, index, expr)
        ControlFlowGraph(Some(previousNode), Some(arrAssignNode),
          List(previousNode, arrAssignNode),
          List(Edge(previousNode, arrAssignNode)))
      case Println(expr) =>
        val exprNode = Nodes.Expression(expr)
        ControlFlowGraph(Some(previousNode), Some(exprNode),
          List(previousNode, exprNode),
          List(Edge(previousNode, exprNode)))
      case While(expr, stat) =>
        val trueNode = Nodes.Expression(expr)
        val falseNode = Nodes.Expression(Not(expr))
        val b1 = Edge(previousNode, trueNode)
        val b2 = Edge(previousNode, falseNode)
        val whileGraph = decomposeStat(stat, trueNode)
        ControlFlowGraph(Some(previousNode),
          Some(falseNode), List(trueNode, falseNode) ++ whileGraph.nodes,
          List(b1, b2) ++ whileGraph.edges)
      case If(expr, thn, els) =>
        val trueNode = Nodes.Expression(expr)
        val falseNode = Nodes.Expression(Not(expr))
        val b1 = Edge(previousNode, trueNode)
        val b2 = Edge(previousNode, falseNode)
        val thenGraph = decomposeStat(thn, trueNode)
        val elsGraph = els match{
          case Some(stat) => decomposeStat(stat, falseNode)
          case None => emptyGraph()
        }
        val mergeNode = Nodes.Merge()

        ControlFlowGraph(Some(previousNode), Some(mergeNode),
          List(trueNode, falseNode) ++ thenGraph.nodes ++ elsGraph.nodes,
          List(b1,b2) ++ thenGraph.edges ++ elsGraph.edges)

      case Block(stats) =>
        var localPrev = previousNode
        stats.map(stat => {
          val g = decomposeStat(stat, localPrev)
          localPrev = g.end.get
          g
        }).foldLeft(emptyGraph())(_ :: _) //maybe foldright
    }

  }


  def decomposeExpression(expr: ExprTree): ControlFlowGraph = expr match {
    case MethodCall(obj, meth, args) =>
      decomposeMethod(program.classes.find(cls => cls.id.value == obj.getType.toString).get.methods.find(met => met.id.value == meth.value).get)
  }

  def decomposeMethod(meth: MethodDecl): ControlFlowGraph ={
    meth.stats.map(stat => {
      decomposeStat(stat)
    }).foldLeft(emptyGraph())(_ :: _)

  }

}
