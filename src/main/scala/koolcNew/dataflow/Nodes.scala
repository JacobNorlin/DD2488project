package koolcNew.dataflow

import koolcNew.ast.Trees.{Identifier, ExprTree}
import koolcNew.dataflow.Edges.{Edge}

object Nodes {

  sealed trait Node

  case class Branch(prev: Option[Node], var next: List[Node]) extends Node
  case class Expression(prev: Option[Node], expr: ExprTree, var next: Option[Node]) extends Node
  case class ArrayAssign(prev: Option[Node], id: Identifier, expr: ExprTree, index: ExprTree, var next: Option[Node]) extends Node
  case class Assign(prev: Option[Node], id: Identifier, expr: ExprTree, var next: Option[Node]) extends Node
  case class Merge(prev: List[Node], var next: Option[Node]) extends Node
  case class ControlFlowNode(prev: Option[Node], name: String, var next:Option[Node]) extends Node

  
}
