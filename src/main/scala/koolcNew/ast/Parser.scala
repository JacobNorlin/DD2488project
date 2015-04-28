package koolcNew
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
      val firstOfExpression = List(STRLITKIND, INTLITKIND, TRUE, FALSE, IDKIND, NEW, BANG, LPAREN, THIS)


      def statDecl:StatTree = currentToken.kind match {
        case IDKIND => {
          val id = findIdentifier //will get pos of currentToken
          readToken
          var ret:StatTree = null
          if(currentToken.kind.equals(LBRACKET)){
            readToken
            val expr1 = expression
            eat(RBRACKET)
            eat(EQSIGN)
            val expr2 = expression
            eat(SEMICOLON)

            ret = ArrayAssign(id, expr1, expr2).setPos(id)

          }else if(currentToken.kind.equals(EQSIGN)){
            readToken
            val expr = expression
            eat(SEMICOLON)
            ret = Assign(id, expr).setPos(id)
          }else{
            expected(EQSIGN, RBRACKET)
          }
          ret

        }
        case WHILE => {
          val pos = currentToken
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          While(expr, statDecl).setPos(pos)
        }
        case PRINTLN => {
          val pos = currentToken
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          eat(SEMICOLON)
          Println(expr).setPos(pos)
        }
        case IF => {
          val pos = currentToken
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          val ifStatement = statDecl
          var elseStatement: Option[StatTree] = None
          if(currentToken.kind.equals(ELSE)){
            readToken
            elseStatement = Some(statDecl)
          }
          If(expr, ifStatement, elseStatement).setPos(pos)
        }
        case LBRACE => {
          val pos = currentToken
          readToken
          var statList: List[StatTree] = List()
          while(firstOfStatement.contains(currentToken.kind)){
            statList = statList++List(statDecl)
          }
          eat(RBRACE)
          Block(statList).setPos(pos)
        }
        case _ => {
          expected(IF, WHILE, PRINTLN, LBRACE, IDKIND)
        }
      }


      def typeDecl: TypeTree = {
        val pos = currentToken
        currentToken.kind match {
          case INT => {

            readToken

            var returnType: TypeTree = IntType()
            if(currentToken.kind.equals(LBRACKET)){
              readToken
              eat(RBRACKET)
              returnType = IntArrayType().setPos(pos)
            }
            returnType
          }
          case BOOLEAN => {
            readToken
            BooleanType().setPos(pos)
          }
          case STRING => {
            readToken
            StringType().setPos(pos)
          }
          case IDKIND => {
            val returnType: TypeTree = findIdentifier
            readToken
            returnType.setPos(pos)
          }
          case _ => {
            expected(INT, BOOLEAN, STRING, IDKIND)
          }
        }
      }
      
      def expression : ExprTree = {
        var ret:ExprTree = null
        ret = expr1
        while(currentToken.kind.equals(OR)) {
          readToken
          ret = Or(ret, expr1).setPos(ret)
        }
        ret
      }
      
      def expr1 : ExprTree = {
        var ret:ExprTree = null
        ret = expr2
        while(currentToken.kind.equals(AND)) {
          readToken
          ret = And(ret, expr2).setPos(ret)
        }
        ret
      }
      
      def expr2 : ExprTree = {
        var ret:ExprTree = null
        ret = expr3
        while(currentToken.kind.equals(EQUALS)||currentToken.kind.equals(LESSTHAN)) {
          if(currentToken.kind.equals(EQUALS)) {
            readToken
            ret = Equals(ret, expr3).setPos(ret)
          } else {
            readToken
            ret = LessThan(ret, expr3).setPos(ret)
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
            ret = Plus(ret, expr4).setPos(ret)
          } else {
            readToken
            ret = Minus(ret, expr4).setPos(ret)
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
            ret = Times(ret, expr5).setPos(ret)
          } else {
            readToken
            ret = Div(ret, expr5).setPos(ret)
          }
        }
        ret
      }
      
      def expr5 : ExprTree = {
        var ret : ExprTree = null
        currentToken.kind match{
        case INTLITKIND => {
          ret = IntLit(currentToken.asInstanceOf[INTLIT].value).setPos(currentToken)
          readToken
        }
        case STRLITKIND => {
          ret = StringLit(currentToken.asInstanceOf[STRLIT].value).setPos(currentToken)
          readToken
        }
        case IDKIND => {
          ret = findIdentifier.setPos(currentToken)
          readToken
        }
        case NEW => {
          val pos = currentToken
          readToken
          if(currentToken.kind.equals(INT)){
            readToken
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
          ret.setPos(pos)
        }
        case BANG => {
          val pos = currentToken
          readToken
          ret = Not(expression).setPos(pos)
        }
        case LPAREN => {
          val pos = currentToken
          readToken
          ret = expression.setPos(pos)
          eat(RPAREN)
        }
        case TRUE => {
          val pos = currentToken
          readToken
          ret = True().setPos(pos)
        }
        case FALSE => {
          val pos = currentToken
          readToken
          ret = False().setPos(pos)
        }
        case THIS => {
          val pos = currentToken
          readToken
          ret = This().setPos(pos)
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
          expressionP(ArrayRead(exprIn, expr).setPos(exprIn))
        }
        case DOT =>{
          readToken
          val pos = currentToken //It makes more sense to take the keyword over the DOT for position
          var ret:ExprTree = null
          if(currentToken.kind.equals(LENGTH)){
            ret = ArrayLength(exprIn).setPos(pos)
            readToken
          }else if(currentToken.kind.equals((IDKIND))){
            val id = findIdentifier
            readToken
            eat(LPAREN)
            var args = List[ExprTree]()
            if(firstOfExpression.contains(currentToken.kind)){
              args = args ++ List(expression)
              while(currentToken.kind.equals(COMMA)){
                readToken
                args = args ++ List(expression)
              }
            }
            eat(RPAREN)
            ret = MethodCall(exprIn, id, args).setPos(pos)

          }else{
            expected(LENGTH, IDKIND)
          }

          expressionP(ret)
        }
        case _ => {
          exprIn
        }
      }

      def findIdentifier: Identifier = {
        if(currentToken.kind.equals(IDKIND)){
          Identifier(currentToken.asInstanceOf[ID].value).setPos(currentToken)
        }else{
          expected(IDKIND)
        }
      }
      def varDecl: VarDecl = {
        val pos = currentToken
        eat(VAR)

        val id = findIdentifier
        readToken
        eat(COLON)
        val varType = typeDecl
        eat(SEMICOLON)
        VarDecl(varType, id).setPos(pos)

      }

      def formalDecl: List[Formal] = {
        var parList:List[Formal] = List()

        var returnValue: List[Formal] = List()

        if(currentToken.kind.equals(IDKIND)){
          val id = findIdentifier
          readToken

          eat(COLON)
          val typeTree = typeDecl

          parList = parList++List(Formal(typeTree, id).setPos(id))

          while(currentToken.kind.equals(COMMA)){
            readToken
            val id = findIdentifier
            readToken

            eat(COLON)
            val typeTree = typeDecl

            parList = parList++List(Formal(typeTree, id).setPos(id))
          }

          returnValue = parList
        }

        returnValue



      }

      def methodDecl: MethodDecl = {
        val pos = currentToken
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

        var varList: List[VarDecl] = List()
        var statList:List[StatTree] = List()

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

        MethodDecl(retType, id, parList, varList, statList, expr).setPos(pos)

      }

      def classDecl: ClassDecl = {
        val pos = currentToken
        eat(CLASS)
        val id = findIdentifier
        readToken

        def findExtends: Option[Identifier] = {
          var ret:Option[Identifier] = None
          if(currentToken.kind.equals(EXTENDS)){
            eat(EXTENDS)
            ret = Option(findIdentifier)
            readToken
          }
          ret
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
        ClassDecl(id, ext, varList, methodList).setPos(pos)
      }

      def mainDecl = {
        val pos = currentToken
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

        MainObject(id, statList).setPos(pos)
      }

      val mainObject = mainDecl

      var classList = List[ClassDecl]()

      while(currentToken.kind.equals(CLASS)){
        classList = classList ++ List(classDecl)
      }
      eat(EOF)
      Program(mainObject, classList).setPos(mainObject)

    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
