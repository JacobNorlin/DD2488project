package koolcNew.dataflow

import koolcNew.ast.Trees.{Identifier, ExprTree}
import koolcNew.dataflow.Edges.{Edge}

object Nodes {

  sealed trait Node

  case class Branch(var prev: Option[Node] = None, var next: List[Int] = List()) extends Node
  case class Expression(expr: ExprTree, var prev: Option[Node] = None,  var next: Option[Int] = None) extends Node
  case class ArrayAssign(id: Identifier, expr: ExprTree, index: ExprTree, var prev: Option[Node] = None, var next: Option[Int] = None) extends Node
  case class Assign(id: Identifier, expr: ExprTree, var prev: Option[Node] = None, var next: Option[Int] = None) extends Node
  case class Merge(var prev: List[Node] = List(), var next: Option[Int] = None) extends Node
  case class ControlFlowNode(name: String, var prev: Option[Node] = None, var next:Option[Int] = None) extends Node
  case class Return(callSite: Int, var next:Option[Int] = None) extends Node

  
}
