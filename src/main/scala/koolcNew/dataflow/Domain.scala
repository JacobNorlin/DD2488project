package koolcNew.dataflow

import koolcNew.ast.Trees.VarDecl
import koolcNew.dataflow.Nodes.{Expression, ArrayAssign, Assign, Node}
import scala.collection.mutable

/**
 * Created by jacob on 2015-06-03.
 */
object Domain {

  sealed trait State

  case class Top() extends State
  case class Bottom() extends State

  sealed trait Domain{
    val top: Top = Top()
    val bottom: Bottom = Bottom()
  }

  case class nullDereference() extends Domain{
    def transfer(node: Node, facts: mutable.Map[Int, State]): mutable.Map[Int, State] = {
      for((v, s) <- facts){
        node match{
          case as: Assign =>
            if(as.id.getSymbol.id  == v)
              facts(v) = Bottom()
          case _ => //do nothing

        }
      }
      facts
    }
  }

}
