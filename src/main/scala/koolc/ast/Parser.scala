package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {

        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {

      val firstOfStatement = List(IDKIND, LBRACE, WHILE, IF, PRINTLN)
      val firstOfExpression = List(STRLITKIND, INTLITKIND, TRUE, FALSE, IDKIND, NEW, BANG, LPAREN)


      def statDecl:StatTree = currentToken.kind match {
        case IDKIND => {
          val id = findIdentifier
          readToken
          var ret:StatTree = null
          if(currentToken.kind.equals(LBRACKET)){
            readToken
            val expr1 = expression
            eat(RBRACKET)
            eat(EQSIGN)
            val expr2 = expression
            eat(SEMICOLON)

            ret = ArrayAssign(id, expr1, expr2)

          }else if(currentToken.kind.equals(EQSIGN)){
            readToken
            val expr = expression
            eat(SEMICOLON)
            ret = Assign(id, expr)
          }else{
            expected(EQSIGN, RBRACKET)
          }
          ret

        }
        case WHILE => {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          While(expr, statDecl)
        }
        case PRINTLN => {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          eat(SEMICOLON)
          Println(expr)
        }
        case IF => {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          val ifStatement = statDecl
          var elseStatement: Option[StatTree] = null
          if(currentToken.kind.equals(ELSE)){
            elseStatement = Option(statDecl)
          }
          If(expr, ifStatement, elseStatement)
        }
        case LBRACE => {
          readToken
          var statList: List[StatTree] = List[StatTree]()
          while(firstOfStatement.contains(currentToken.kind)){
            statList = statList++List(statDecl)
          }
          eat(RBRACE)
          Block(statList)
        }
        case _ => {
          expected(IF, WHILE, PRINTLN, LBRACE, IDKIND)
        }
      }


      def typeDecl: TypeTree = currentToken.kind match {
        case INT => {
          readToken

          var returnType: TypeTree = IntType()
          if(currentToken.kind.equals(LBRACKET)){
            readToken
            eat(RBRACKET)
            returnType = IntArrayType()
          }
          returnType
        }
        case BOOLEAN => {
          readToken
          BooleanType()
        }
        case STRING => {
          readToken
          StringType()
        }
        case IDKIND => {
          readToken
          findIdentifier
        }
        case _ => {
          expected(INT, BOOLEAN, STRING, IDKIND)
        }
      }
      def expression: ExprTree = currentToken.kind match{
        case INTLITKIND => {
          val ret = IntLit(currentToken.asInstanceOf[INTLIT].value)
          readToken
          expressionP(ret)
        }
        case STRLITKIND => {
          val ret = StringLit(currentToken.asInstanceOf[STRLIT].value)
          readToken
          ret
        }
        case IDKIND => {
          val ret = findIdentifier
          readToken
          ret
        }
        case NEW => {
          readToken
          var ret:ExprTree = null
          if(currentToken.kind.equals(INT)){
            eat(LBRACKET)
            val expr = expression
            eat(RBRACKET)
            ret = NewIntArray(expr)
          }else if(currentToken.kind.equals(IDKIND)){
            val id = findIdentifier
            readToken
            eat(LPAREN)
            eat(RPAREN)
            ret = New(id)
          }else{
            expected(INT, IDKIND)
          }
          ret
        }
        case BANG => {
          readToken
          Not(expression)
        }
        case LPAREN => {
          val expr = expression
          eat(RPAREN)
          expr
        }
        case TRUE => {
          readToken
          True()
        }
        case FALSE => {
          readToken
          False()
        }
        case THIS => {
          readToken
          This()
        }

      }

      def expressionP(exprIn:ExprTree): ExprTree = currentToken.kind match {
        case LBRACKET => {
          readToken
          val expr = expression
          eat(RBRACKET)
          ArrayRead(exprIn, expr)
        }
        case DOT =>{
          readToken
          var ret:ExprTree = null
          if(currentToken.kind.equals(LENGTH)){
            ret = ArrayLength(exprIn)
            readToken
          }else if(currentToken.kind.equals((IDKIND))){
            val id = findIdentifier
            readToken
            eat(LPAREN)
            var args = List[ExprTree]()
            while(firstOfExpression.contains(currentToken.kind)){
              args = args ++ List(expression)
            }
            ret = MethodCall(exprIn, id, args)

          }else{
            expected(LENGTH, IDKIND)
          }

          ret
        }
        case PLUS | MINUS | TIMES | DIV => {
          var ret: ExprTree = null
          def term(f1:ExprTree, f2:ExprTree) = {
            if(currentToken.kind.equals(PLUS)){
              Plus(f1, f2)
            }else if()
          }

          def factor(t1:ExprTree, t2:ExprTree) = {


          }
        }

        case _ => {
          exprIn
        }
      }

      def findIdentifier: Identifier = {
        if(currentToken.kind.equals(IDKIND)){
          Identifier(currentToken.toString)
        }else{
          expected(IDKIND)
        }
      }
      def varDecl: VarDecl = {
        eat(VAR)

        val id = findIdentifier
        readToken
        eat(COLON)
        val varType = typeDecl

        VarDecl(varType, id)

      }

      def formalDecl: List[Formal] = {
        var parList:List[Formal] = List[Formal]()

        var returnValue: List[Formal] = null

        if(currentToken.kind.equals(IDKIND)){
          val id = findIdentifier
          readToken

          eat(COLON)
          val typeTree = typeDecl

          parList = parList++List(Formal(typeTree, id))

          while(currentToken.kind.equals(COMMA)){
            val id = findIdentifier
            readToken

            eat(COLON)
            val typeTree = typeDecl

            parList = parList++List(Formal(typeTree, id))
          }

          returnValue = parList
        }

        returnValue



      }

      def methodDecl: MethodDecl = {
        eat(DEF)
        val id =  findIdentifier
        readToken

        eat(LPAREN)
        val parList = formalDecl
        eat(RPAREN)

        eat(COLON)
        val retType = typeDecl

        eat(EQSIGN)
        eat(LBRACE)

        var varList = List[VarDecl]()
        var statList = List[StatTree]()

        while(currentToken == VAR){
          varList = varList++List(varDecl)
        }

        while(firstOfStatement.contains(currentToken.kind)){
          statList = statList++List(statDecl)
        }

        eat(RETURN)

        val expr = expression

        eat(SEMICOLON)
        eat(RBRACE)

        MethodDecl(retType, id, parList, varList, statList, expr)

      }

      def classDecl: ClassDecl = {
        eat(CLASS)
        val id = findIdentifier
        readToken

        def findExtends: Option[Identifier] = {
          if(currentToken == EXTENDS){
            eat(EXTENDS)
            Option(Identifier(currentToken.toString))
          }
          null
        }

        val ext = findExtends

        eat(RBRACE)

        var varList = List[VarDecl]()
        var methodList = List[MethodDecl]()

        while(currentToken == VAR){
          varList = varList++List(varDecl)
        }
        while(currentToken == DEF){
          methodList = methodList++List(methodDecl)
        }

        ClassDecl(id, ext, varList, methodList)
      }

      def mainDecl = {
        eat(OBJECT)
        val id = findIdentifier
        readToken

        eat(LBRACE)
        eat(DEF)
        eat(MAIN)
        eat(LPAREN)
        eat(RPAREN)
        eat(COLON)
        eat(UNIT)
        eat(EQSIGN)
        eat(LBRACE)

        var statList = List[StatTree]()

        while(firstOfStatement.contains(currentToken.kind)){
          statList = statList++List(statDecl)
        }

        eat(RBRACE)
        eat(RBRACE)

        MainObject(id, statList)
      }

      val mainObject = mainDecl

      var classList = List[ClassDecl]()

      while(currentToken.kind.equals(CLASS)){
        classList = classList ++ List(classDecl)
      }
      eat(EOF)
      Program(mainObject, classList)

    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
