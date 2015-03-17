package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

import scala.runtime.Nothing$

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
    var column = Position.column(Position.FIRSTPOS);
    var line = Position.line(Position.FIRSTPOS);

    //println("!====="+source.next())
    var currentToken = source.next()

    import ctx.reporter._


    def readNextToken:Char = {
      column += 1

      if(currentToken.equals('\n')){
        line+=1
        column = 0
      }
      var token = -1.toChar
      if(source.hasNext){
        token = source.next()
      }
      token
    }


    // Complete this file

    new Iterator[Token] {
      def hasNext = {

        !(currentToken.toByte == -1)

      }

      def next = {
        val currentWord = new StringBuffer()

        var returnToken:Token = new Token(Tokens.BAD)

        while(currentToken.isWhitespace) {
          currentToken = readNextToken
        }

        //COMMENTS
        if(currentToken.equals('/')) {
          currentToken = readNextToken
          if (currentToken.equals('/')) {
            //Normal comment
            while (!currentToken.equals('\n') && !(currentToken.toByte == -1)) {
              currentToken = readNextToken
            }
            returnToken = next
          }else if (currentToken.equals('*')) { //start block comment
            //Block comments
            var flag = true
            while (!(currentToken.toByte == -1) && flag) {

              currentToken = readNextToken
              if (currentToken.equals('*')) {
                currentToken = readNextToken
                if (currentToken.equals('/')){
                  currentToken = readNextToken
                  flag = false
                }
              }
            }
            returnToken = next

          } else {
            // Only the '/' char, means return DIVISION token
            returnToken = new Token(Tokens.DIV)
          }
          //Read in the next bit after the comment


        }
        //KEYWORDS AND IDENTIFIERS
        else if(currentToken.isLetter){
          //println("Current token is identifier or keyword")
          currentWord.append(currentToken);
          currentToken = readNextToken
          while(currentToken.isLetterOrDigit){
            currentWord.append(currentToken)
            currentToken = readNextToken

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
          currentToken = readNextToken
          while(!currentToken.equals('"') && !currentToken.equals('\n')){
            currentWord.append(currentToken)
            currentToken = readNextToken
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
            currentToken = readNextToken
          }
          returnToken = new INTLIT(k)
        }
        //SPECIAL CHARS

        else if(!currentToken.isLetterOrDigit){
          returnToken = currentToken match {
            case '(' => new Token(Tokens.LPAREN)
            case ')' => new Token(Tokens.RPAREN)
            case '=' => {currentToken = readNextToken
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
            case '&' => {currentToken = readNextToken
              if(currentToken.equals('&')){
                new Token(Tokens.AND)
              }else{
                new Token(Tokens.BAD)
              }
            }
            case '|' => {currentToken = readNextToken
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
          currentToken = readNextToken
        }

        //Check if end of file
        if(currentToken.toByte == -1){
          returnToken = new Token(Tokens.EOF)
        }

        val pos = Position.encode(line, column);
        returnToken.setPos(ctx.file, source.pos)
        returnToken


      }
    }

  }
}
