

import java.io.File

import koolcNew.analyzer.NameAnalysis
import testUtils._
import koolcNew.ast._
import koolcNew.lexer.Lexer
import koolcNew.utils.Context
import org.scalatest._


/**
 * Created by jacob on 2015-04-27.
 */
class NameAnalysisTest extends FlatSpec{




    val testDir = new File("testprograms/valid").listFiles()
    val pipeline = Lexer andThen Parser andThen NameAnalysis
    val refPipeline = koolc.lexer.Lexer andThen koolc.ast.Parser andThen koolc.analyzer.NameAnalysis
    for(file <- testDir){
      "ourProg in "+file.getName should "be equal to refProg" in {


        val ctx = Context(reporter = new koolcNew.utils.Reporter(), file = file, outDir = None)
        val refCtx = koolc.utils.Context(reporter = new koolc.utils.Reporter(), files = List(new File("testprograms/lab3/valid/Factorial.kool")), outDir = None)
        val program = pipeline.run(ctx)(ctx.file)
        val refProgram = refPipeline.run(refCtx)(ctx.file)


        val ids = getAllIds(program)
        val refIds = getAllIds(refProgram)
        val names = getAllNames(program)
        val refNames = getAllNames(refProgram)


        /* This counts the number of occurences of each symbol. Since where the symbol ids
        are attached  varies between the reference compiler and our compiler, this should serve
        as a decent approximation of similarity between symbols. If number of symbol occurences
        are the same between the programs, then for large programs the probability of the symbols
        being exactly the same increasees
         */
        val idControl = ids.map(id => ids.count(_ == id)).sortBy(x=>x)
        val refIdControl = refIds.map(id => refIds.count(_ == id)).sortBy(x=>x)

        //Check that the same amount of sybmols exists, and they are atleast in some approximation the same
        assert(ids.length === refIds.length)
        assert(idControl === refIdControl)
        //Checks that varName are the same
        assert(names === refNames)


      }


    }





}
