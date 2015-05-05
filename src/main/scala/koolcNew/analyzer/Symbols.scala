package koolcNew
package analyzer

import utils._
import Types._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = {
      val c = classes.getOrElse(n, None)
      var r: Option[ClassSymbol] = None
      if (c != None) {
        r = Some(c.asInstanceOf[ClassSymbol])
      }
      r
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = {
      val m = methods.getOrElse(n, None)
      var r: Option[MethodSymbol] = None
      if (m == None && parent != None)
        r = Option(parent.get.lookupMethod(n).orNull)
      else if (m != None)
        r = Some(m.asInstanceOf[MethodSymbol])
      r
    }
    def lookupVar(n: String): Option[VariableSymbol] = {
      val m = members.getOrElse(n, None)
      var r: Option[VariableSymbol] = None
      if (m != None)
        r = Some(m.asInstanceOf[VariableSymbol])
      else if (parent != None)
        r = Option(parent.get.lookupVar(n).orNull)
      r
    }

  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None


    /*This only checks local variables, as that was convenient for the codegeneration
    To check everything do something like:

    methodSymbol.lookupVar(value).getOrElse(methodSymbol.classSymbol.lookupVar(value).orNull))
     */
    def lookupVar(n: String): Option[VariableSymbol] = {
      val p = params.getOrElse(n, None)
      val m = members.getOrElse(n, None)
      var r: Option[VariableSymbol] = None

      if (p != None)
        r = Some(p.asInstanceOf[VariableSymbol])
      else if (m != None)
        r = Some(m.asInstanceOf[VariableSymbol])

      r
    }
  }

  class VariableSymbol(val name: String) extends Symbol {
    var used: Boolean = false
  }
}
