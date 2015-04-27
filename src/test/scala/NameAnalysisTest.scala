

import java.io.File

import koolcNew.analyzer.NameAnalysis
import koolcNew.ast
import koolcNew.ast.Trees._
import koolcNew.ast._
import koolcNew.lexer.Lexer
import koolcNew.utils.Context
import org.scalatest._


/**
 * Created by jacob on 2015-04-27.
 */
class NameAnalysisTest extends FlatSpec{



  "ourProg" should "be equal to refProg" in {

    val testDir = new File("testprograms/valid").listFiles()
    val pipeline = Lexer andThen Parser andThen NameAnalysis
    val refPipeline = koolc.lexer.Lexer andThen koolc.ast.Parser andThen koolc.analyzer.NameAnalysis
    for(file <- testDir){


      val ctx = Context(reporter = new koolcNew.utils.Reporter(), file = file, outDir = None)
      val refCtx = koolc.utils.Context(reporter = new koolc.utils.Reporter(), files = List(new File("testprograms/lab3/valid/Factorial.kool")), outDir = None)
      val program = pipeline.run(ctx)(ctx.file)
      val refProgram = refPipeline.run(refCtx)(ctx.file)


      val ids = getAllIds(program)
      val refIds = getAllIds(refProgram)
      val names = getAllNames(program)
      val refNames = getAllNames(refProgram)
      println(file.getName+": "+names, file.getName+": "+refNames)
      println(file.getName+": "+names, file.getName+": "+refNames)


      //assert(program.toString === koolc.ast.ASTPrinter(refProgram))
      assert(ids === refIds)
      assert(names === refNames)

    }


  }
  def getAllNames(prog: Program): List[String]= {
    def getArgs(met: MethodDecl): List[String] = {
      if (met.args != null) {
        met.args.map(arg => arg.getSymbol.name)
      } else {
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.name) ++
        cls.vars.map(v => v.getSymbol.name) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.name) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.name)
        })
    }).sortBy(x => x)
  }

  def getAllNames(prog: koolc.ast.Trees.Program): List[String]= {
    def getArgs(met: koolc.ast.Trees.MethodDecl): List[String] = {
      if (met.args != null) {
        met.args.map(arg => arg.getSymbol.name)
      } else {
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.name) ++
        cls.vars.map(v => v.getSymbol.name) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.name) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.name)
        })
    }).sortBy(x => x)
  }


  def getAllIds(prog: Program): List[Int]= {
    def getArgs(met:MethodDecl):List[Int] = {
      if(met.args != null){
        met.args.map(arg => arg.getSymbol.id)
      }else{
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.id) ++
        cls.vars.map(v => v.getSymbol.id) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.id) ++
            getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.id)
        })
    }).sortBy(x => x)

  }
  def getAllIds(prog: koolc.ast.Trees.Program): List[Int] = {
    def getArgs(met: koolc.ast.Trees.MethodDecl): List[Int] = {
      if (met.args != null) {
        met.args.map(arg => arg.getSymbol.id)
      } else {
        List()
      }
    }
    prog.classes.flatMap(cls => {
      List(cls.getSymbol.id) ++
        cls.vars.map(v => v.getSymbol.id) ++
        cls.methods.flatMap(met => {
          List(met.getSymbol.id) ++
          getArgs(met) ++
            met.vars.map(v1 => v1.getSymbol.id)
        })
    }).sortBy(x => x)

  }


//  def convertTree(t: koolc.ast.Trees.Program) = {
//    def convertClasses(classes: List[koolc.ast.Trees.ClassDecl]): List[ClassDecl] = {
//      var clses:List[ClassDecl]
//      for(cls <- classes){
//        clses.::(cls.)
//      }
//    }
//    var program = Program(MainObject(t.main.id), )
//  }

}
