package koolcNew.dataflow

import koolcNew.ast.Trees._
import koolcNew.dataflow.Edges._
import koolcNew.dataflow.Nodes.{ControlFlowNode, Node}
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
      def ++(edge: Edge) = addEdge(this, edge)

      def ::(cfg: ControlFlowGraph) = append(this, cfg)
      override def toString: String = "{"+start+", "+end+" }"
    }

    def addEdge(cfg: ControlFlowGraph, edge: Edge): ControlFlowGraph ={
      ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, cfg.edges ++ List(edge))
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

    val startNode: Node = Nodes.ControlFlowNode(None, "start", None)
    val endNode = Nodes.ControlFlowNode(None, "end", None)
    var graph = ControlFlowGraph(Some(startNode), Some(endNode), List(startNode, endNode), List())
    var methodMap = Map[Tuple2[ClassDecl, MethodDecl], ControlFlowGraph]()

    graph = buildGraph(prog)
    println("Graph", graph)



    def buildGraph(prog: Program): ControlFlowGraph = {
//
//      val metGraphs = prog.classes.flatMap(cls => {
//        cls.methods.map(met => {
//          val metGraph = decomposeMethod(ControlFlowGraph(Some(ControlFlowNode(None, "", None)), Some(ControlFlowNode(None, "", None)), List(), List()), met)
//          methodMap = methodMap + ((cls, met) -> metGraph)
//        })
//      })



      var previousNode = startNode

      //Create a graph for each statement
      val progGraph = prog.main.stats.map(stat => {
        val g = decomposeStat(stat, previousNode)
        previousNode = g.end.get
        g
      }).foldRight(graph)((cfg1, cfg2) => { //Add all graphs together
        cfg1 :: cfg2
      })

      progGraph

    }


      def setNextNode(trg: Node, next: Node): Unit = {
        trg match {
          case exprNode: Nodes.Expression =>
            exprNode.next = Some(next)
          case assignNode: Nodes.Assign =>
            assignNode.next = Some(next)
          case arrAssignNode: Nodes.ArrayAssign =>
            arrAssignNode.next = Some(next)
          case branchNode: Nodes.Branch =>
            branchNode.next = branchNode.next ++ List(next)
          case mergeNode: Nodes.Merge =>
            mergeNode.next = Some(next)
          case cfgNode: Nodes.ControlFlowNode =>
            println(Some(next))
            cfgNode.next = Some(next)


        }
      }
    def decomposeStat(stat: StatTree, previousNode: Node): ControlFlowGraph = {
      stat match {
        case Assign(id, expr) =>
          val assignNode = Nodes.Assign(Some(previousNode), id, expr, None)

          setNextNode(previousNode, assignNode)

          ControlFlowGraph(Some(previousNode),
              Some(assignNode), List(previousNode, assignNode),
            List(Edge(previousNode, assignNode)))
        case ArrayAssign(id, index, expr) =>
          val arrAssignNode = Nodes.ArrayAssign(Some(previousNode), id, index, expr, None)

          setNextNode(previousNode, arrAssignNode)

          ControlFlowGraph(Some(previousNode), Some(arrAssignNode),
            List(previousNode, arrAssignNode),
            List(Edge(previousNode, arrAssignNode)))
        case Println(expr) =>
          val exprNode = Nodes.Expression(Some(previousNode), expr, None)
          setNextNode(previousNode, exprNode)

          ControlFlowGraph(Some(previousNode), Some(exprNode),
            List(previousNode, exprNode),
            List(Edge(previousNode, exprNode)))
        case While(expr, stat) =>
          val branchNode = Nodes.Branch(Some(previousNode), List())
          setNextNode(previousNode, branchNode)
          val prevEdge = Edge(previousNode, branchNode)

          val trueNode = Nodes.Expression(Some(branchNode), expr, None)
          val falseNode = Nodes.Expression(Some(branchNode), Not(expr), None)
          val b1 = Edge(branchNode, trueNode)
          val b2 = Edge(branchNode, falseNode)
          val whileGraph = decomposeStat(stat, trueNode)
          ControlFlowGraph(Some(branchNode),
            Some(falseNode), List(trueNode, falseNode, branchNode) ++ whileGraph.nodes,
            List(b1, b2, prevEdge) ++ whileGraph.edges)
        case If(expr, thn, els) =>
          val branchNode = Nodes.Branch(Some(previousNode), List())
          setNextNode(previousNode, branchNode)

          val prevEdge = Edge(previousNode, branchNode)

          val trueNode = Nodes.Expression(Some(branchNode), expr, None)
          val falseNode = Nodes.Expression(Some(branchNode), Not(expr), None)

          val b1 = Edge(branchNode, trueNode)
          val b2 = Edge(branchNode, falseNode)

          val thenGraph = decomposeStat(thn, trueNode)
          val elsGraph = els match {
            case Some(stat) => decomposeStat(stat, falseNode)
            case None => emptyGraph()
          }

          val mergeNode = elsGraph.end match {
            case Some(node) => Nodes.Merge(List(thenGraph.end.get, node), None)
            case None => Nodes.Merge(List(thenGraph.end.get), None)
          }

          val merge1 = Edge(thenGraph.end.getOrElse(trueNode), mergeNode)
          val merge2 = Edge(elsGraph.end.getOrElse(falseNode), mergeNode)
          ControlFlowGraph(Some(branchNode), Some(mergeNode),
            List(trueNode, falseNode, mergeNode, branchNode) ++ thenGraph.nodes ++ elsGraph.nodes,
            List(b1, b2, merge1, merge2, prevEdge) ++ thenGraph.edges ++ elsGraph.edges)

        case Block(stats) =>
          var localPrev = previousNode
          stats.map(stat => {
            val g = decomposeStat(stat, localPrev)
            println(g)
            localPrev = g.end.get
            g
          }).foldRight(new ControlFlowGraph(Some(localPrev), None, List(), List()))(_ :: _)
      }




    }

    def linkMethodCalls(cfg: ControlFlowGraph): Unit ={
      for(node <- cfg.nodes){
        (node match {
          case Nodes.Expression(prev, expr, next) =>
            decomposeExpression(expr)
          case Nodes.ArrayAssign(prev, _, expr, index, next) =>
            decomposeExpression(expr)
            decomposeExpression(index)
          case Nodes.Assign(prev, _, expr, next) =>
            decomposeExpression(expr)
        }) match {
          case Some(g) =>
            //ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, cfg.edges++g.edges++List(Edge(node, g.start), Edge(g.end, ))

        }


      }
    }


    def decomposeExpression(expr: ExprTree): Option[ControlFlowGraph]= expr match {
      case MethodCall(obj, meth, args) =>
        val clsDecl = prog.classes.find(cls => cls.id.value==obj.getType.toString).get
        val metDecl = clsDecl.methods.find(met => met.id.value == meth.value).get
        methodMap.get((clsDecl, metDecl))
      case _ => None
    }

    def decomposeMethod(base: ControlFlowGraph, meth: MethodDecl): ControlFlowGraph = {

      var previousNode = base.start.get
      meth.stats.map(stat => {
        val g = decomposeStat(stat, previousNode)
        previousNode = g.end.get
        g
      }).foldRight(base)(_ :: _)

    }

    prog
  }

}
