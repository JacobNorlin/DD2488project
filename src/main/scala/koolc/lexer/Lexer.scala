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
    val charStream = source.bufferedReader()

    var currentToken = charStream.read().toChar

    import ctx.reporter._


    // Complete this file

    new Iterator[Token] {
      def hasNext = {

        !(currentToken.toByte == -1)

      }

      def next = {
        val currentWord = new StringBuffer()

        var returnToken:Token = new ID("null")

        while(currentToken.isWhitespace) {
          currentToken = charStream.read().toChar
        }

        //COMMENTS
        if(currentToken.equals('/')){
          currentToken = charStream.read().toChar
          if(currentToken.equals('/')){//Normal comment
            while(!currentToken.equals('\n') && !(currentToken.toByte == -1)){
              currentToken = charStream.read().toChar
            }
          }else if(currentToken.equals('*')){//Block comments
            var flag = true
            while(!(currentToken.toByte == -1) && flag) {
              currentToken = charStream.read().toChar
              if(currentToken.equals('*')) {
                currentToken = charStream.read().toChar
                if(currentToken.equals('/')) flag = false
              }
            }
          }else{// Only the '/' char, means return DIVISION token
            returnToken = new Token(Tokens.DIV)
          }
          //Read in the next bit after the comment


        }
        //KEYWORDS AND IDENTIFIERS
        else if(currentToken.isLetter){
          currentWord.append(currentToken);
          currentToken = charStream.read().toChar
          while(currentToken.isLetterOrDigit){
            currentWord.append(currentToken)
            currentToken = charStream.read().toChar

          }
          //Get the tokenkind of the word
          val tokenKind = getKeywordToken(currentWord.toString)


          //Check if tokenkind is identifier or keyword
          if(tokenKind.equals(Tokens.IDKIND)){
            returnToken = new ID(currentWord.toString)
          }else{
            //New token representing keyword
            returnToken = new Token(tokenKind)
          }

        }

        //STRING LITERALS
        else if(currentToken.equals('"')){
          currentToken = charStream.read().toChar
          while(!currentToken.equals('"') && !currentToken.equals('\n')){
            currentWord.append(currentToken)
            currentToken = charStream.read().toChar
          }
          if(currentToken.equals('"')){
            returnToken = new STRLIT(currentWord.toString)
          }else{
            returnToken = new ID("error")
          }
        }
        //INT LITERALS
        else if(currentToken.isDigit){
          var k = 0

          while(currentToken.isDigit){
            k = 10*k + currentToken.toString.toInt
            currentToken = charStream.read().toChar
          }
          returnToken = new INTLIT(k)
        }
        //SPECIAL CHARS
        else if(!currentToken.isLetterOrDigit){
          returnToken = currentToken match {
            case '(' => new Token(Tokens.LPAREN)
            case ')' => new Token(Tokens.RPAREN)
            case '=' => {currentToken = charStream.read().toChar
              if(currentToken.equals('=')){
                new Token(Tokens.EQUALS)
              }else{
                new Token(Tokens.EQSIGN)
              }
            }
            case ':' => new Token(Tokens.COLON)
            case ';' => new Token(Tokens.SEMICOLON)
            case '.' => new Token(Tokens.DOT)
            case ',' => new Token(Tokens.COMMA)
            case '!' => new Token(Tokens.BANG)
            case '[' => new Token(Tokens.LBRACKET)
            case ']' => new Token(Tokens.RBRACKET)
            case '{' => new Token(Tokens.LBRACE)
            case '}' => new Token(Tokens.RBRACE)
            case '&' => {currentToken = charStream.read().toChar
              if(currentToken.equals('&')){
                new Token(Tokens.AND)
              }else{
                new Token(Tokens.BAD)
              }
            }
            case '|' => {currentToken = charStream.read().toChar
              if(currentToken.equals('&')){
                new Token(Tokens.OR)
              }else{
                new Token(Tokens.BAD)
              }
            }
            case '<' => new Token(Tokens.LESSTHAN)
            case '+' => new Token(Tokens.PLUS)
            case '-' => new Token(Tokens.MINUS)
            case '*' => new Token(Tokens.TIMES)
            case _ => new Token(Tokens.BAD)

          }
          currentToken = charStream.read().toChar
        }



        returnToken


      }
    }

  }
}
