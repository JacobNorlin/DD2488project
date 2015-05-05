package koolcNew

import koolcNew.analyzer.NameAnalysis
import koolcNew.code.CodeGeneration
import utils._
import java.io.File
import koolcNew.analyzer.TypeChecking

import lexer._
import ast._

object Main {

  var tokenFlag = false;
  var printFlag = false;

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = {
      args match {
        case "-d" :: out :: args =>
          outDir = Some(new File(out))
          processOption(args)
        case "--tokens" :: args =>
          tokenFlag = true;
          processOption(args)
        case "--print" :: args =>
          printFlag = true
          processOption(args)

        case f ::args =>
          files = new File(f) :: files
          processOption(args)




        case Nil =>
      }
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }


  def main(args: Array[String]) {

    val pipelineTokens= Lexer andThen PrintTokens andThen Parser andThen NameAnalysis andThen TypeChecking
    val pipelineFrontEnd = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
    val pipelineBackend = pipelineFrontEnd andThen CodeGeneration


    val ctx = processOptions(args)

    var program: Trees.Program = null

    if(tokenFlag)
      program = pipelineTokens.run(ctx)(ctx.file)

    if(printFlag){
      if(program == null)
        program = pipelineFrontEnd.run(ctx)(ctx.file)
      println(Printer(program))
    }
    if(program == null){
      pipelineBackend.run(ctx)(ctx.file)
    }



  }
}
