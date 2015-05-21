package koolcNew.dataflow

import koolcNew.ast.Trees.ExprTree
import koolcNew.dataflow.Nodes.Node
import koolcNew.utils.Positioned

object Edges {

  sealed trait Statement extends Positioned

  case class PrintLnEdge(srcNode: Node,  expr: ExprTree, trgNode: Node) extends Statement
  /* How are branches done? An if edge will contain statements for both then and else. When an if is
  encountered inside the AST we will create two nodes, and an edge for each. Should they be differentiated?
  How is the bool-expr stored? As an edge to the if node? V-(boolExp)-V<=. An if statement seems more like
  a construction of 3 nodes and 2 edges.
   */
  
}
