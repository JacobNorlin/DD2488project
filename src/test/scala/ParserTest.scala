import java.io.File

import _root_.testUtils._
import koolcNew.analyzer.NameAnalysis
import koolcNew.ast.Parser
import koolcNew.lexer.Lexer
import koolcNew.utils.Context
import org.scalatest.FlatSpec
import testUtils._

/**
 * Created by jacob on 2015-04-28.
 */
class ParserTest extends FlatSpec{

  "ourProg" should "be equal to refProg" in {

    val testDir = new File("testprograms/valid").listFiles()
    val pipeline = Lexer andThen Parser andThen NameAnalysis
    val refPipeline = koolc.lexer.Lexer andThen koolc.ast.Parser andThen koolc.analyzer.NameAnalysis
    for (file <- testDir) {


      val ctx = Context(reporter = new koolcNew.utils.Reporter(), file = file, outDir = None)
      val refCtx = koolc.utils.Context(reporter = new koolc.utils.Reporter(), files = List(new File("testprograms/lab3/valid/Factorial.kool")), outDir = None)
      val program = pipeline.run(ctx)(ctx.file)
      val refProgram = refPipeline.run(refCtx)(ctx.file)



      //Check that the AST is the same for both programs
      assert(program.toString === koolc.ast.Printer(refProgram))
      println(file.getName + " NameAnalysis passed")


    }
  }

}
