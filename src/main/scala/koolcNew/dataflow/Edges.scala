package koolcNew.dataflow

import koolcNew.ast.Trees.StatTree
import koolcNew.ast.Trees.{Identifier, ExprTree}
import koolcNew.dataflow.Nodes.Node
import koolcNew.utils.Positioned

object Edges {

  case class Edge(src: Node, trg: Node)


}
