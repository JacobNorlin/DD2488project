package koolcNew.dataflow

import koolcNew.dataflow.Edges.Statement

object Nodes {


  case class Node(edges: List[Statement])
  
}
