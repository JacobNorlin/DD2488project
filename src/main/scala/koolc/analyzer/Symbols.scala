package koolc
package analyzer

import utils._

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

  sealed abstract class Symbol extends Positioned {
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
    var classes = Map[String,ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = {
      val c = classes.find({case (k,v) => k==n}).get
      var r:Option[ClassSymbol] = null
      if(c != null)
        r = Option(c._2)
      r
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String,MethodSymbol]()
    var members = Map[String,VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = {
      val m = methods.find({case (k,v) => k==n}).get
      var r:Option[MethodSymbol] = null
      if(m != null)
        r = Option(m._2)
      else
        r = parent.get.lookupMethod(n)
      r
    }
    def lookupVar(n: String): Option[VariableSymbol] = {
      val m = members.find({case (k,v) => k==n}).get
      var r:Option[VariableSymbol] = null
      if(m != null)
        r = Option(m._2)
      else
        r = parent.get.lookupVar(n)
      r
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var members = Map[String,VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      val p = params.find({case (k,v) => k==n}).get
      val m = members.find({case (k,v) => k==n}).get

      var r:Option[VariableSymbol] = null

      if (p == null)
        r = Option(m._2)
      else if(m == null)
        r = Option(p._2)
      else
        r = classSymbol.lookupVar(n)
      r

    }
  }

  class VariableSymbol(val name: String) extends Symbol
}
