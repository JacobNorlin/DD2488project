package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

import scala.runtime.Nothing$

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def getKeywordToken(keyWord: String): TokenKind = {
    keyWord match {
      case "object" => Tokens.OBJECT
      case "class" => Tokens.CLASS
      case "def" => Tokens.DEF
      case "var" => Tokens.VAR
      case "Unit" => Tokens.UNIT
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

    var currentChar = source.next()
    var EOFFound = false;
    val EOFChar = -1.toChar

    import ctx.reporter._

    def readNextToken: Char = {

      var token = -1.toChar
      if (source.hasNext) {
        token = source.next()
      }

      token
    }

    // Complete this file

    new Iterator[Token] {
      def hasNext = {
        !EOFFound
      }

      def next = {
        val currentWord = new StringBuffer()
        var divFound = false
        var errorMessage = ""

        var returnToken: Token = new Token(Tokens.BAD)

        //COMMENTS
        //println("currentChar at start of next: " + currentChar)



        var continue = false;
        do {
          while (currentChar.isWhitespace) {
            currentChar = readNextToken
          }

          //Set that there is no more input
          if (currentChar.toByte == -1) {
            EOFFound = true
          }

          //COMMENTS
          if (currentChar.equals('/')) {
            currentChar = readNextToken
            if (currentChar.equals('/')) {
              //Normal comment
              while (!currentChar.equals('\n') && !(currentChar.toByte == -1)) {
                currentChar = readNextToken
              }
              continue = true
            } else if (currentChar.equals('*')) { //start block comment
              //Block comments
              var flag = true
              while (!(currentChar.toByte == -1) && flag) {


                currentChar = readNextToken
                if (currentChar.equals('*')) {
                  currentChar = readNextToken
                  if (currentChar.equals('/')) {
                    currentChar = readNextToken
                    flag = false
                  }
                }
              }
              continue = true
            } else {
              // Only the '/' char, means return DIVISION token
              returnToken = new Token(Tokens.DIV)
              divFound = true
            }

          } else { continue = false }
        } while (continue)
        //KEYWORDS AND IDENTIFIERS
        if(!divFound){
          if (currentChar.isLetter) {
            currentWord.append(currentChar)
            currentChar = readNextToken
            while (currentChar.isLetterOrDigit || currentChar.equals('_')) {
              currentWord.append(currentChar)
              currentChar = readNextToken

            }

            //Get the tokenkind of the word

            val tokenKind = getKeywordToken(currentWord.toString)

            //Check if tokenkind is identifier or keyword
            if (tokenKind.equals(Tokens.IDKIND)) {
              returnToken = new ID(currentWord.toString)
            } else {
              //New token representing keyword
              returnToken = new Token(tokenKind)
            }

          } //STRING LITERALS
          else if (currentChar.equals('"')) {
            currentChar = readNextToken
            while (!currentChar.equals('"') && !currentChar.equals('\n') && !currentChar.equals(-1.toChar)) {
              currentWord.append(currentChar)
              currentChar = readNextToken
            }
            if (currentChar.equals('"')) {
              returnToken = new STRLIT(currentWord.toString)
            } else {
              returnToken = new Token(Tokens.BAD)
              errorMessage = "Missing closing \""
            }
            //Read in next charachter to avoid running again with bad o
            currentChar = readNextToken
          } //INT LITERALS
          else if (currentChar.isDigit) {
            var k = 0

            while (currentChar.isDigit) {
              k = 10 * k + currentChar.toString.toInt
              currentChar = readNextToken
            }
            returnToken = new INTLIT(k)
          } //SPECIAL CHARS
          else if (!currentChar.isLetterOrDigit) {
            var skipRead = true
            returnToken = currentChar match {
              case '(' => new Token(Tokens.LPAREN)
              case ')' => new Token(Tokens.RPAREN)
              case '=' => {
                currentChar = readNextToken
                if (currentChar.equals('=')) {
                  new Token(Tokens.EQUALS)
                } else {
                  skipRead = false
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
              case '&' => {
                currentChar = readNextToken
                if (currentChar.equals('&')) {
                  new Token(Tokens.AND)
                } else {
                  errorMessage = "Incorrect &&"
                  new Token(Tokens.BAD)
                }
              }
              case '|' => {
                currentChar = readNextToken
                if (currentChar.equals('|')) {
                  new Token(Tokens.OR)
                } else {
                  errorMessage = "Incorrect ||"
                  new Token(Tokens.BAD)
                }
              }
              case '<' => new Token(Tokens.LESSTHAN)
              case '+' => new Token(Tokens.PLUS)
              case '-' => new Token(Tokens.MINUS)
              case '*' => new Token(Tokens.TIMES)
              case '/' => new Token(Tokens.DIV)
              case EOFChar => new Token(Tokens.EOF)
              case _ => {
                errorMessage = "Invalid character"
                new Token(Tokens.BAD)
              }


            }
            if(skipRead){
              currentChar = readNextToken
            }

          }
        }


        returnToken.setPos(ctx.file, source.pos)
        if(returnToken.kind.equals(Tokens.BAD)){
          error(errorMessage, returnToken)
        }

        returnToken

      }
    }

  }
}
