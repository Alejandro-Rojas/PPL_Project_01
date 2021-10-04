/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 */

/*
mouse       = { statement } ´$$´
statement   = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
string      = ´"´ { character } ´"´
identifier  = letter
literal     = ´0´ | nonzero { digit }
nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
if          = ´[´ { statement } ´]´
while       = ´(´ { statement } ´)´
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´
punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´ | ´^´ | ´&´ | ´(´ | ´)´ | ´-´ | ´+´ | ´=´ | ´'´ | ´/´ | ´\´ | ´[´ | ´]´ | ´{´ | ´}´ | ´|´
blank       = ´ ´
character   = letter | digit | punctuation | special | blank
 */

class SyntaxAnalyzer(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null

  // returns the current lexeme
  private def getLexeme(): Lexeme = {
    if (current == null) {
      current = it.next
    }
    //    println(current)
    current
  }

  // advances the input one lexeme
  private def nextLexeme() = {
    current = it.next
  }

  // TODO: finish the recursive descent parser
  // parses the program, returning its corresponding parse tree
  def parse() = {
    parseMouse()
  }
 def parseMouse() = {
    val tree = new Tree("mouse")
    tree.add(branch = parseMouse())
    while (getLexeme().getToken() == Token.EO_PRG) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(branch = parseMouse())
    }
    tree
  }

  // statement = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
  def parseStatement() = {
    val tree = new Tree("statement")
    var lexeme = getLexeme()
    if (lexeme.getToken() == Token.READ_PUSH) {
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      lexeme = getLexeme()
      if (lexeme.getToken() == Token.POPS_OUTPUT) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.STRING) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.IDENTIFIER) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.POP_STACKS) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
        if (lexeme.getToken() == Token.LITERAL) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.ADD_STACKS) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.SUB_STACKS) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.DIV_STACKS) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.MULT_STACKS) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.RETRIEV_VALUE) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.RETRIEV_VALUE) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.BREAK_LOOP) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.IF) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      if (lexeme.getToken() == Token.WHILE) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(parseStatement())
      }
      else
        throw new Exception("Syntax error: '=' expected!")
    }
    else
      throw new Exception("Syntax error: identifier expected!")
    tree
  }

  // string      = ´"´ { character } ´"´
  def parseString() = {
    val tree = new Tree("while")
    var lexeme = getLexeme()
    if (lexeme.getToken() == Token.OPEN_BRACK) {
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(branch = parseString())
      lexeme = getLexeme()
      if (lexeme.getToken() == Token.CLOSE_BRACK) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
      }
      else
        throw new Exception("Syntax error: ']' expected!")
    }
    else
      throw new Exception("Syntax error: '[' expected!")
    tree
  }


  // identifier  = letter
  def parseIdentifier(): Tree = {
    val tree = new Tree("identifer")
    tree.add(parseIdentifier())
    while (getLexeme().getToken() == Token.LETTER) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseIdentifier())
    }
    tree
  }

  // literal     = ´0´ | nonzero { digit }
  def parseLiteral(): Tree = {
    val tree = new Tree("literal")
    tree.add(parseLiteral())
    while (getLexeme().getToken() == Token.LITERAL) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseLiteral())
    }
    tree
  }

  // if          = ´[´ { statement } ´]´
  def parseIF() = {
    val tree = new Tree("if")
    var lexeme = getLexeme()
    if (lexeme.getToken() == Token.OPEN_BRACK) {
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(branch = parseIF())
      lexeme = getLexeme()
      if (lexeme.getToken() == Token.CLOSE_BRACK) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
      }
      else
        throw new Exception("Syntax error: ')' expected!")
    }
    else
      throw new Exception("Syntax error: '(' expected!")
    tree
  }

// while       = ´(´ { statement } ´)´
  def parseWhile() = {
    val tree = new Tree("while")
    var lexeme = getLexeme()
    if (lexeme.getToken() == Token.OPEN_PAR) {
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(branch = parseWhile())
      lexeme = getLexeme()
      if (lexeme.getToken() == Token.CLOSE_PAR) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
      }
      else
        throw new Exception("Syntax error: ')' expected!")
    }
    else
      throw new Exception("Syntax error: '(' expected!")
    tree
  }
}

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}
