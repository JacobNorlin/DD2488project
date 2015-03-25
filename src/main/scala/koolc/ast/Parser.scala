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
          var returnType: TypeTree = findIdentifier
          readToken
          returnType
        }
        case _ => {
          expected(INT, BOOLEAN, STRING, IDKIND)
        }
      }
      
      def expression : ExprTree = {
        var ret:ExprTree = null
        ret = expr1
        while(currentToken.kind.equals(OR)) {
          readToken
          ret = Or(ret, expr1)
        }
        ret
      }
      
      def expr1 : ExprTree = {
        var ret:ExprTree = null
        ret = expr2
        while(currentToken.kind.equals(AND)) {
          readToken
          ret = And(ret, expr2)
        }
        ret
      }
      
      def expr2 : ExprTree = {
        var ret:ExprTree = null
        ret = expr3
        while(currentToken.kind.equals(EQUALS)||currentToken.kind.equals(LESSTHAN)) {
          if(currentToken.kind.equals(EQUALS)) {
            readToken
            ret = Equals(ret, expr3)
          } else {
            readToken
            ret = LessThan(ret, expr3)
          }
        }
        ret
      }
      
      def expr3 : ExprTree = {
        var ret:ExprTree = null
        ret = expr4
        while(currentToken.kind.equals(PLUS)||currentToken.kind.equals(MINUS)) {
          if(currentToken.kind.equals(PLUS)) {
            readToken
            ret = Plus(ret, expr4)
          } else {
            readToken
            ret = Minus(ret, expr4)
          }
        }
        ret
      }
      
      def expr4 : ExprTree = {
        var ret:ExprTree = null
        ret = expr5
        while(currentToken.kind.equals(TIMES)||currentToken.kind.equals(DIV)) {
          if(currentToken.kind.equals(TIMES)) {
            readToken
            ret = Times(ret, expr5)
          } else {
            readToken
            ret = Div(ret, expr5)
          }
        }
        ret
      }
      
      def expr5 : ExprTree = {
        var ret : ExprTree = null
        currentToken.kind match{
        case INTLITKIND => {
          ret = IntLit(currentToken.asInstanceOf[INTLIT].value)
          readToken
        }
        case STRLITKIND => {
          ret = StringLit(currentToken.asInstanceOf[STRLIT].value)
          readToken
        }
        case IDKIND => {
          ret = findIdentifier
          readToken
        }
        case NEW => {
          readToken
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
        }
        case BANG => {
          readToken
          ret = Not(expression)
        }
        case LPAREN => {
          readToken
          ret = expression
          eat(RPAREN)
        }
        case TRUE => {
          readToken
          ret = True()
        }
        case FALSE => {
          readToken
          ret = False()
        }
        case THIS => {
          readToken
          ret = This()
        }
        case _ => {
          expected(STRLITKIND, INTLITKIND, TRUE, FALSE, IDKIND, NEW, BANG, LPAREN)
        }
      }
        expressionP(ret)
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

        while(currentToken.kind.equals(VAR)){
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
          if(currentToken.kind.equals(EXTENDS)){
            eat(EXTENDS)
            Option(Identifier(currentToken.toString))
          }
          null
        }

        val ext = findExtends

        eat(LBRACE)

        var varList = List[VarDecl]()
        var methodList = List[MethodDecl]()

        while(currentToken.kind.equals(VAR)){
          varList = varList++List(varDecl)
        }
        while(currentToken.kind.equals(DEF)){
          methodList = methodList++List(methodDecl)
        }
        eat(RBRACE)
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
