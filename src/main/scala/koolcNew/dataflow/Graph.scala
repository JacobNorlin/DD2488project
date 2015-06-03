package koolcNew.dataflow

import koolcNew.ast.Trees._
import koolcNew.dataflow.Edges._
import koolcNew.dataflow.Nodes.{ControlFlowNode, Node}
import koolcNew.utils.{Context, Pipeline}

object Graph extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {

    case class ControlFlowGraph(
                                 start: Option[Node] = None,
                                 end: Option[Node] = None,
                                 nodes: Map[Int, Node] = Map[Int, Node](),
                                 edges: List[Edge] = List(),
                                 var methodMap: Map[Tuple2[ClassDecl, MethodDecl], ControlFlowGraph] = Map[Tuple2[ClassDecl, MethodDecl], ControlFlowGraph](),
                                 var nodeCounter: Int = 1
                                 ) {
      def +(node: Node) = addNode(this, node)
      def ++(edge: Edge) = addEdge(this, edge)
      def --(edge: Edge) = removeEdge(this, edge)

      def ::(cfg: ControlFlowGraph) = append(this, cfg)
      override def toString: String = "{"+start+", "+end+" }"
    }


    def addEdge(cfg: ControlFlowGraph, edge: Edge): ControlFlowGraph ={
      ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, cfg.edges ++ List(edge), cfg.methodMap)
    }



    /**
     * Create edge from n1 to n2
     * @param n1
     * @param n2
     */
    def createEdge(n1: Node, n2:Node) = {
      val e = Edge(n1,n2)
      setNextNode(e.src,Some(e.trg.idNum))
      e
    }


    def emptyGraph() = {
      ControlFlowGraph()
    }

    def addNode(cfg: ControlFlowGraph, node: Node): ControlFlowGraph = {
      node.idNum = cfg.nodeCounter
      ControlFlowGraph(cfg.start, cfg.end, cfg.nodes + (node.idNum -> node), cfg.edges, cfg.methodMap, cfg.nodeCounter+1)
    }

    def append(cfg1: ControlFlowGraph, cfg2: ControlFlowGraph) = {
      ControlFlowGraph(cfg1.start, cfg2.end, cfg1.nodes ++ cfg2.nodes, cfg1.edges ++ cfg2.edges, cfg1.methodMap ++ cfg2.methodMap)
    }


    def removeEdge(cfg: ControlFlowGraph, edge: Edge) : ControlFlowGraph = {
      setNextNode(edge.src, None)
      ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, cfg.edges diff List(edge), cfg.methodMap)
    }
    buildGraph(prog)


    def buildGraph(prog: Program): ControlFlowGraph = {
      val endNode: Node = Nodes.ControlFlowNode("end")
      val startNode: Node = Nodes.ControlFlowNode("start")
      val graph = ControlFlowGraph(Some(startNode), Some(endNode))
      graph + startNode
      graph + endNode

      val metGraphs = prog.classes.flatMap(cls => {
        cls.methods.map(met => {
          val metGraph = decomposeMethod(ControlFlowGraph(Some(ControlFlowNode("")), Some(ControlFlowNode(""))), met)
          graph.methodMap = graph.methodMap + ((cls, met) -> metGraph)
          metGraph
        })
      })




      var previousNode = startNode

      //Create a graph for each statement
      var progGraph = prog.main.stats.map(stat => {
        val g = decomposeStat(stat, previousNode)
        previousNode = g.end.get
        g
      }).foldRight(graph)((cfg1, cfg2) => { //Add all graphs together
        cfg1 :: cfg2
      })

      for(met <- metGraphs){

        for((key, node) <- met.nodes){
          progGraph = progGraph + node
        }
      }
      //Link the final node to the end node
      progGraph ++ createEdge(previousNode, endNode)

      println(progGraph.nodes)

      progGraph = linkMethodCalls(progGraph)

      //print da graph

      def graphprinter(node: Option[Int]): Int = {

        if(node != None){

          println(node)

          progGraph.nodes.get(node.get).get match {
            case nd: Nodes.Assign => println(nd)
              graphprinter(nd.next)
            case nd: Nodes.ArrayAssign => println(nd)
              graphprinter(nd.next)
            case nd: Nodes.Expression => println(nd)
              graphprinter(nd.next)
            case nd: Nodes.Return => println(nd)
              graphprinter(nd.next)
            case nd: Nodes.Branch => println(nd)
              for(br <- nd.next)
                graphprinter(Some(br))

            case nd: Nodes.Merge => println(nd)
              graphprinter(nd.next)
            case nd: Nodes.ControlFlowNode => println(nd)
              graphprinter(nd.next)

          }
        }
        0
      }
      graphprinter(Some(progGraph.start.get.idNum))

      progGraph

    }


    def setNextNode(trg: Node, nxt: Option[Int]): Unit= {
      trg match {
        case exprNode: Nodes.Expression =>
          exprNode.next = nxt
        case assignNode: Nodes.Assign =>
          assignNode.next = nxt
        case arrAssignNode: Nodes.ArrayAssign =>
          arrAssignNode.next = nxt
        case branchNode: Nodes.Branch =>
          nxt match {
            case Some(n) =>
              branchNode.next = branchNode.next ++ List(n)
            case None =>
              branchNode.next = List()

          }
        case mergeNode: Nodes.Merge =>
          mergeNode.next = nxt
        case cfgNode: Nodes.ControlFlowNode =>
          cfgNode.next = nxt
        case retNode: Nodes.Return =>
          retNode.next = nxt
      }
    }

    def decomposeStat(stat: StatTree, previousNode: Node): ControlFlowGraph = {
      stat match {
        case Assign(id, expr) =>
          val assignNode = Nodes.Assign(id, expr)
          val e = createEdge(previousNode, assignNode)

          ControlFlowGraph(Some(previousNode),
              Some(assignNode), Map(),
            List(e)) + assignNode + previousNode
        case ArrayAssign(id, index, expr) =>
          val arrAssignNode = Nodes.ArrayAssign(id, index, expr)
          val e = createEdge(previousNode, arrAssignNode)


          ControlFlowGraph(Some(previousNode), Some(arrAssignNode),
            Map(),
            List(e)) + arrAssignNode + previousNode
        case Println(expr) =>
          val exprNode = Nodes.Expression(expr)
          val e = createEdge(previousNode, exprNode)

          ControlFlowGraph(Some(previousNode), Some(exprNode),
            Map(),
            List(e))+ previousNode + exprNode
        case While(expr, stat) =>
          val branchNode = Nodes.Branch()

          val e = createEdge(previousNode, branchNode)

          val trueNode = Nodes.Expression(expr)
          val falseNode = Nodes.Expression(Not(expr))
          val b1 = createEdge(branchNode, trueNode)
          val b2 = createEdge(branchNode, falseNode)

          val whileGraph = decomposeStat(stat, trueNode)
          ControlFlowGraph(Some(branchNode),
            Some(falseNode), whileGraph.nodes,
            List(b1, b2, e) ++ whileGraph.edges) + trueNode + falseNode + branchNode
        case If(expr, thn, els) =>
          //Create branch point
          val branchNode = Nodes.Branch()

          val e = createEdge(previousNode, branchNode)

          val trueNode = Nodes.Expression(expr)
          val falseNode = Nodes.Expression(Not(expr))

          val b1 = createEdge(branchNode, trueNode)
          val b2 = createEdge(branchNode, falseNode)

          val thenGraph = decomposeStat(thn, trueNode)
          val elsGraph = els match {
            case Some(stat) => decomposeStat(stat, falseNode)
            case None => emptyGraph()
          }

          //Create merge point
          val mergeNode = Nodes.Merge()
          val thenMerge = createEdge(thenGraph.end.getOrElse(trueNode), mergeNode)

          val elseMerge = elsGraph.end match {
            case Some(node) => createEdge(node, mergeNode)
            case None => createEdge(falseNode, mergeNode)
          }

          ControlFlowGraph(Some(branchNode), Some(mergeNode),
            thenGraph.nodes ++ elsGraph.nodes,
            List(b1, b2, thenMerge, elseMerge, e) ++ thenGraph.edges ++ elsGraph.edges) + trueNode + falseNode + mergeNode + branchNode

        case Block(stats) =>
          var localPrev = previousNode
          stats.map(stat => {
            val g = decomposeStat(stat, localPrev)
            println(g)
            localPrev = g.end.get
            g
          }).foldRight(new ControlFlowGraph(Some(localPrev)))(_ :: _)
      }




    }

    def linkMethodCalls(cfg: ControlFlowGraph): ControlFlowGraph ={
      for(node <- cfg.nodes) {

        node match {
          case exprNode: Nodes.Expression =>
            val metGraph = decomposeExpression(exprNode.expr, cfg.methodMap)

            metGraph match {
              case Some(g) =>
                val ret = Nodes.Return(exprNode.idNum)
                val nextNode = cfg.nodes.get(exprNode.next.get).get //muh options
                cfg -- Edge(exprNode, nextNode)
                cfg + ret
                println(s"Linking methodcall at $exprNode with $ret")
                cfg ++ createEdge(exprNode, ret)
                cfg ++ createEdge(exprNode, g.start.get)
                cfg ++ createEdge(g.end.get, ret)
                cfg ++ createEdge(ret, nextNode)
              case None =>

            }
          case arrAssignNode: Nodes.ArrayAssign =>
            val exprGraph = decomposeExpression(arrAssignNode.expr, cfg.methodMap)
            val indexGraph = decomposeExpression(arrAssignNode.index, cfg.methodMap)
            (exprGraph, indexGraph) match {
              case (Some(g1), Some(g2)) =>
                println(s"Linking methodcall at $arrAssignNode")
                val ret = Nodes.Return(arrAssignNode.idNum)
                val nextNode = cfg.nodes.get(arrAssignNode.next.get).get
                cfg -- Edge(arrAssignNode, nextNode)
                cfg + ret
                cfg ++ createEdge(arrAssignNode, ret)
                cfg ++ createEdge(arrAssignNode, g1.start.get)
                cfg ++ createEdge(arrAssignNode, g2.start.get)
                cfg ++ createEdge(g1.end.get, arrAssignNode)
                cfg ++ createEdge(g2.end.get, arrAssignNode)
                cfg ++ createEdge(ret, nextNode)
              case _ => None
            }
          case assignNode: Nodes.Assign =>
            val assignGraph = decomposeExpression(assignNode.expr, cfg.methodMap)

            assignGraph match {
              case Some(g) =>
                println(s"Linking methodcall at $assignNode")
                val ret = Nodes.Return(assignNode.idNum)
                val nextNode = cfg.nodes.get(assignNode.next.get).get
                cfg -- Edge(assignNode, nextNode)
                cfg + ret
                cfg ++ createEdge(assignNode, ret)
                cfg ++ createEdge(assignNode, g.start.get)
                cfg ++ createEdge(g.end.get, assignNode)
                cfg ++ createEdge(ret, nextNode)
            }
          case _ => //Do nothing
        }



      }
      cfg
    }


    def decomposeExpression(expr: ExprTree, methodMap: Map[Tuple2[ClassDecl, MethodDecl], ControlFlowGraph]): Option[ControlFlowGraph]= expr match {
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
