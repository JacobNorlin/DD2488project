import java.io.File
import koolcNew.analyzer.NameAnalysis
import koolcNew.ast.Parser
import koolcNew.lexer.Lexer
import koolcNew.utils.Context
import org.scalatest.FlatSpec

/**
 * Created by jacob on 2015-04-28.
 */
class ParserTest extends FlatSpec{



    val testDir = new File("testprograms/valid").listFiles()
    val pipeline = Lexer andThen Parser andThen NameAnalysis
    val refPipeline = koolc.lexer.Lexer andThen koolc.ast.Parser andThen koolc.analyzer.NameAnalysis
    for (file <- testDir) {
      "ourProg in "+file.getName should "be equal to refProg" in {
        val ctx = Context(reporter = new koolcNew.utils.Reporter(), file = file, outDir = None)
        val refCtx = koolc.utils.Context(reporter = new koolc.utils.Reporter(), files = List(new File("testprograms/lab3/valid/Factorial.kool")), outDir = None)
        val program = pipeline.run(ctx)(ctx.file)
        val refProgram = refPipeline.run(refCtx)(ctx.file)



        //Check that the AST is the same for both programs
        //println(program.toString. == koolc.ast.ASTPrinter(refProgram))
        assert(program.toString === koolc.ast.ASTPrinter(refProgram))

      }
    }

}
