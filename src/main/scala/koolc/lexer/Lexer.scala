package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._


  def getKeywordToken(keyWord: String): TokenKind ={
    keyWord match {
      case "object" => Tokens.OBJECT
      case "class" => Tokens.CLASS
      case "def" => Tokens.DEF
      case "var" => Tokens.VAR
      case "unit" => Tokens.UNIT
      case "main" => Tokens.MAIN
      case "string" => Tokens.STRING
      case "extends" => Tokens.EXTENDS
      case "int" => Tokens.INT
      case "boolean" => Tokens.BOOLEAN
      case "while" => Tokens.WHILE
      case "if" => Tokens.IF
      case "else" => Tokens.ELSE
      case "return" => Tokens.RETURN
      case "length" => Tokens.LENGTH
      case "true" => Tokens.TRUE
      case "false" => Tokens.FALSE
      case "this" => Tokens.THIS
      case "new" => Tokens.NEW
      case "println" => Tokens.PRINTLN
      case _ => Tokens.IDKIND
    }
  }


  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    val charStream = source.bufferedReader();



    import ctx.reporter._

    // Complete this file

    new Iterator[Token] {
      def hasNext = {
        true;
      }

      def next = {
        var currentToken = charStream.read().toChar;
        val currentWord = new StringBuffer();

        var returnToken:Token = new ID("null")

        while(currentToken.isWhitespace) {
          currentToken = charStream.read().toChar
        }

        //COMMENTS
        if(currentToken.equals('/')){
          currentToken = charStream.read().toChar
          if(currentToken.equals('/')){//Nomral comment
            while(!currentToken.equals('\n') && !(currentToken == -1)){
              currentToken = charStream.read().toChar
            }
          }else if(currentToken.equals('*')){//Block comments
          val secondToken = charStream.read().toChar
            while(!currentToken.equals('*') && secondToken.equals('/')){
              currentToken = charStream.read().toChar
            }
          }else{// Only the '/' char, mean return DIVISION token
          }
          //Read in the next bit after the comment
          currentToken = charStream.read().toChar;

        }
        
        //KEYWORDS AND IDENTIFIERS
        if(currentToken.isLetter){
          currentWord.append(currentToken);
          currentToken = charStream.read().toChar;
          while(currentToken.isLetterOrDigit){
            currentWord.append(currentToken);
            currentToken = charStream.read().toChar;
          }
          val t = getKeywordToken(currentWord.toString)



          if(t.equals(Tokens.IDKIND)){
            returnToken = new ID(currentWord.toString)
          }else{
            returnToken = new KEYWORD(currentWord.toString)
          }
        }

        //STRING LITERALS
        if(currentToken.equals('"')){
          currentToken = charStream.read().toChar;
          while(!currentToken.equals('"') && !currentToken.equals('\n')){
            currentWord.append(currentToken);
            currentToken = charStream.read().toChar;
          }
          if(currentToken.equals('"')){
            returnToken = new STRLIT(currentWord.toString)
          }else{
            returnToken = new ID("error");
          }
        }
        //INT LITERALS
        if(currentToken.isDigit){
          var k = 0

          while(currentToken.isDigit){
            k = 10*k + currentToken.toString.toInt;
            currentToken = charStream.read().toChar
          }
          returnToken = new INTLIT(k)
        }


        returnToken


      }
    }

  }
}
