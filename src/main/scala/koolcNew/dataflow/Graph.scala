package koolcNew.dataflow

import koolcNew.ast.Trees._
import koolcNew.dataflow.Edges._
import koolcNew.dataflow.Nodes.Node
import koolcNew.utils.{Context, Pipeline}

object Graph extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {

    case class ControlFlowGraph(
                                 start: Option[Node],
                                 end: Option[Node],
                                 nodes: List[Node],
                                 edges: List[Edge]
                                 ) {
      def +(node: Node) = addNode(this, node)

      def ::(cfg: ControlFlowGraph) = append(this, cfg)
      override def toString: String = "{"+start+", "+end+" }"
    }

    def emptyGraph() = {
      ControlFlowGraph(None, None, List(), List())
    }

    def addNode(cfg: ControlFlowGraph, node: Node): ControlFlowGraph = {
      ControlFlowGraph(cfg.start, cfg.end, cfg.nodes ++ List(node), cfg.edges)
    }

    def append(cfg1: ControlFlowGraph, cfg2: ControlFlowGraph) = {
      ControlFlowGraph(cfg1.start, cfg2.end, cfg1.nodes ++ cfg2.nodes, cfg1.edges ++ cfg2.edges)
    }

    val startNode = Nodes.ControlFlowNode()
    val endNode = Nodes.ControlFlowNode()
    val startEdge = Edge(startNode, endNode)
    val graph = ControlFlowGraph(Some(startNode), Some(endNode), List(startNode, endNode), List(startEdge))

    var previousNode = startNode

    buildGraph(prog)


    def buildGraph(prog: Program): Unit = {
      val metGraphs = prog.classes.flatMap(cls => {
        cls.methods.map(met => decomposeMethod(met))
      })

      val progGraph = prog.main.stats.map(stat => decomposeStat(stat, graph.start.get)).
        foldLeft(graph)(_ :: _)

      for(node <- progGraph.nodes)
        println(node)


    }


    def decomposeStat(stat: StatTree, previousNode: Node): ControlFlowGraph = {
      stat match {
        case Assign(id, expr) =>
          val callGraph = decomposeExpression(expr)
          val assignNode = Nodes.Assign(id, expr)
          println("Assignnode", assignNode)
          val gr = ControlFlowGraph(Some(previousNode),
            Some(assignNode), List(previousNode, assignNode),
            List(Edge(previousNode, assignNode)))
          callGraph match {
            case Some(g) =>
              g :: graph
            case None =>
              graph
          }
        case ArrayAssign(id, index, expr) =>
          val callGraph = decomposeExpression(expr)
          val arrAssignNode = Nodes.ArrayAssign(id, index, expr)
          val gr = ControlFlowGraph(Some(previousNode), Some(arrAssignNode),
            List(previousNode, arrAssignNode),
            List(Edge(previousNode, arrAssignNode)))
          callGraph match {
            case Some(g) =>
              g :: graph
            case None =>
              graph
          }
        case Println(expr) =>
          val callGraph = decomposeExpression(expr)
          val exprNode = Nodes.Expression(expr)
          val gr = ControlFlowGraph(Some(previousNode), Some(exprNode),
            List(previousNode, exprNode),
            List(Edge(previousNode, exprNode)))
          callGraph match {
            case Some(g) =>
              g :: graph
            case None =>
              graph
          }
        case While(expr, stat) =>
          val callGraph = decomposeExpression(expr)
          val trueNode = Nodes.Expression(expr)
          val falseNode = Nodes.Expression(Not(expr))
          val b1 = Edge(previousNode, trueNode)
          val b2 = Edge(previousNode, falseNode)
          val whileGraph = decomposeStat(stat, trueNode)
          val gr =  ControlFlowGraph(Some(previousNode),
            Some(falseNode), List(trueNode, falseNode) ++ whileGraph.nodes,
            List(b1, b2) ++ whileGraph.edges)
          callGraph match {
            case Some(g) =>
              g :: graph
            case None =>
              graph
          }
        case If(expr, thn, els) =>
          val callGraph = decomposeExpression(expr)
          val trueNode = Nodes.Expression(expr)
          val falseNode = Nodes.Expression(Not(expr))
          val b1 = Edge(previousNode, trueNode)
          val b2 = Edge(previousNode, falseNode)
          val thenGraph = decomposeStat(thn, trueNode)
          val elsGraph = els match {
            case Some(stat) => decomposeStat(stat, falseNode)
            case None => emptyGraph()
          }
          val mergeNode = Nodes.Merge()
          val gr = ControlFlowGraph(Some(previousNode), Some(mergeNode),
            List(trueNode, falseNode) ++ thenGraph.nodes ++ elsGraph.nodes,
            List(b1, b2) ++ thenGraph.edges ++ elsGraph.edges)
          callGraph match {
            case Some(g) =>
              g :: graph
            case None =>
              graph
          }

        case Block(stats) =>
          var localPrev = previousNode
          stats.map(stat => {
            val g = decomposeStat(stat, localPrev)
            println(stat, g)
            localPrev = g.end.get
            g
          }).foldLeft(emptyGraph())(_ :: _) //maybe foldright
      }

    }


    def decomposeExpression(expr: ExprTree): Option[ControlFlowGraph]= expr match {
      case MethodCall(obj, meth, args) =>
        Some(decomposeMethod(prog.classes.find(cls => cls.id.value == obj.getType.toString).get.methods.find(met => met.id.value == meth.value).get))
      case _ => None
    }

    def decomposeMethod(meth: MethodDecl): ControlFlowGraph = {
      meth.stats.map(stat => {
        decomposeStat(stat, new Nodes.ControlFlowNode())
      }).foldLeft(emptyGraph())(_ :: _)

    }

    prog
  }

}
