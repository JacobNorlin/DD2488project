package koolcNew.dataflow

import koolcNew.ast.Trees.{Identifier, ExprTree}
import koolcNew.dataflow.Edges.{Edge}

object Nodes {

  sealed trait Node

  case class Branch(edges: List[Edge]) extends Node
  case class Expression(expr: ExprTree) extends Node
  case class ArrayAssign(id: Identifier, expr: ExprTree, index: ExprTree) extends Node
  case class Assign(id: Identifier, expr: ExprTree) extends Node
  case class Merge() extends Node
  case class ControlFlowNode() extends Node

  
}
