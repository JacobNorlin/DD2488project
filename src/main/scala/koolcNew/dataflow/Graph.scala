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

    val startNode: Node = Nodes.ControlFlowNode("start")
    val endNode = Nodes.ControlFlowNode("end")
    var graph = ControlFlowGraph(Some(startNode), Some(endNode), List(startNode, endNode), List())
    var methodMap = Map[Tuple2[ClassDecl, MethodDecl], ControlFlowGraph]()

    graph = buildGraph(prog)
    println("Graph", graph)



    def buildGraph(prog: Program): ControlFlowGraph = {

      for(cls <- prog.classes){
        for(met <- cls.methods){
          val start = new ControlFlowNode(met.id.value+"Start")
          val end = new ControlFlowNode(met.id.value+"End")
          methodMap = methodMap + ((cls, met) -> new ControlFlowGraph(Some(start), Some(end), List(), List()))
        }
      }


      val metGraphs = prog.classes.flatMap(cls => {
        cls.methods.map(met => {
          decomposeMethod(methodMap.get((cls,met)).get, met)
        })
      })



      var previousNode = startNode

      //Create a graph for each statement
      val progGraph = prog.main.stats.map(stat => {
        val g = decomposeStat(stat, previousNode)
        previousNode = g.end.get
        g
      }).foldRight(graph)((cfg1, cfg2) => { //Add all graphs together
        cfg1 :: cfg2
      })

      for(met <- metGraphs){
        println(met.edges)
      }

      progGraph

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
              (g :: gr) ++ Edge(previousNode, g.start.get)
            case None =>
              gr
          }
        case ArrayAssign(id, index, expr) =>
          val callGraph = decomposeExpression(expr)
          val arrAssignNode = Nodes.ArrayAssign(id, index, expr)
          val gr = ControlFlowGraph(Some(previousNode), Some(arrAssignNode),
            List(previousNode, arrAssignNode),
            List(Edge(previousNode, arrAssignNode)))
          callGraph match {
            case Some(g) =>
              g :: gr ++ Edge(previousNode, g.start.get)
            case None =>
              gr
          }
        case Println(expr) =>
          val callGraph = decomposeExpression(expr)
          val exprNode = Nodes.Expression(expr)
          val gr = ControlFlowGraph(Some(previousNode), Some(exprNode),
            List(previousNode, exprNode),
            List(Edge(previousNode, exprNode)))
          callGraph match {
            case Some(g) =>
              g :: gr ++ Edge(previousNode, g.start.get)
            case None =>
              gr
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
              g :: gr ++ Edge(previousNode, g.start.get)
            case None =>
              gr
          }
        case If(expr, thn, els) =>
          val callGraph = decomposeExpression(expr)
          val trueNode = Nodes.Expression(expr)
          val falseNode = Nodes.Expression(Not(expr))
          val b1 = Edge(previousNode, trueNode)
          val b2 = Edge(previousNode, falseNode)
          val thenGraph = decomposeStat(thn, trueNode)
          println("THENGRAPH", thenGraph)
          val elsGraph = els match {
            case Some(stat) => decomposeStat(stat, falseNode)
            case None => emptyGraph()
          }
          val mergeNode = Nodes.Merge()
          val merge1 = Edge(thenGraph.end.getOrElse(trueNode), mergeNode)
          val merge2 = Edge(elsGraph.end.getOrElse(falseNode), mergeNode)
          val gr = ControlFlowGraph(Some(previousNode), Some(mergeNode),
            List(trueNode, falseNode, mergeNode) ++ thenGraph.nodes ++ elsGraph.nodes,
            List(b1, b2, merge1, merge2) ++ thenGraph.edges ++ elsGraph.edges)
          callGraph match {
            case Some(g) =>
              g :: gr ++ Edge(previousNode, g.start.get)
            case None =>
              gr
          }

        case Block(stats) =>
          var localPrev = previousNode
          println("Starting new BLOCK")
          stats.map(stat => {
            val g = decomposeStat(stat, localPrev)
            println(g)
            localPrev = g.end.get
            g
          }).foldRight(new ControlFlowGraph(Some(localPrev), None, List(), List()))(_ :: _)
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
