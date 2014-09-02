package um

import scala.annotation.tailrec
import Um._

object Uh {

  // Unicode preliminaries /////////////////////////////////////////////////////
  // TODO: Process code points rather than chars
  // TODO: does anything else behave like square brackets?
  // TODO: for delimPairs, match up corresponding
  //  START_PUNCTUATION - END_PUNCTUATION and
  //  INITIAL_QUOTE_PUNCTUATION - FINAL_QUOTE_PUNCTUATION pairs in:
  //
  // 0x28: LEFT PARENTHESIS
  // 0x29: RIGHT PARENTHESIS
  // 0x5b: LEFT SQUARE BRACKET
  // 0x5d: RIGHT SQUARE BRACKET
  // 0x7b: LEFT CURLY BRACKET
  // 0x7d: RIGHT CURLY BRACKET
  // 0xab: LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  // 0xbb: RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  // 0xf3a: TIBETAN MARK GUG RTAGS GYON
  // 0xf3b: TIBETAN MARK GUG RTAGS GYAS
  // 0xf3c: TIBETAN MARK ANG KHANG GYON
  // 0xf3d: TIBETAN MARK ANG KHANG GYAS
  // 0x169b: OGHAM FEATHER MARK
  // 0x169c: OGHAM REVERSED FEATHER MARK
  // 0x2018: LEFT SINGLE QUOTATION MARK
  // 0x2019: RIGHT SINGLE QUOTATION MARK
  // 0x201a: SINGLE LOW-9 QUOTATION MARK
  // 0x201b: SINGLE HIGH-REVERSED-9 QUOTATION MARK
  // 0x201c: LEFT DOUBLE QUOTATION MARK
  // 0x201d: RIGHT DOUBLE QUOTATION MARK
  // 0x201e: DOUBLE LOW-9 QUOTATION MARK
  // 0x201f: DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  // 0x2039: SINGLE LEFT-POINTING ANGLE QUOTATION MARK
  // 0x203a: SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
  // 0x2045: LEFT SQUARE BRACKET WITH QUILL
  // 0x2046: RIGHT SQUARE BRACKET WITH QUILL
  // 0x207d: SUPERSCRIPT LEFT PARENTHESIS
  // 0x207e: SUPERSCRIPT RIGHT PARENTHESIS
  // 0x208d: SUBSCRIPT LEFT PARENTHESIS
  // 0x208e: SUBSCRIPT RIGHT PARENTHESIS
  // 0x2329: LEFT-POINTING ANGLE BRACKET
  // 0x232a: RIGHT-POINTING ANGLE BRACKET
  // 0x2768: MEDIUM LEFT PARENTHESIS ORNAMENT
  // 0x2769: MEDIUM RIGHT PARENTHESIS ORNAMENT
  // 0x276a: MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
  // 0x276b: MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
  // 0x276c: MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
  // 0x276d: MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
  // 0x276e: HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
  // 0x276f: HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
  // 0x2770: HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
  // 0x2771: HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
  // 0x2772: LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
  // 0x2773: LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
  // 0x2774: MEDIUM LEFT CURLY BRACKET ORNAMENT
  // 0x2775: MEDIUM RIGHT CURLY BRACKET ORNAMENT
  // 0x27c5: LEFT S-SHAPED BAG DELIMITER
  // 0x27c6: RIGHT S-SHAPED BAG DELIMITER
  // 0x27e6: MATHEMATICAL LEFT WHITE SQUARE BRACKET
  // 0x27e7: MATHEMATICAL RIGHT WHITE SQUARE BRACKET
  // 0x27e8: MATHEMATICAL LEFT ANGLE BRACKET
  // 0x27e9: MATHEMATICAL RIGHT ANGLE BRACKET
  // 0x27ea: MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
  // 0x27eb: MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
  // 0x27ec: MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
  // 0x27ed: MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
  // 0x27ee: MATHEMATICAL LEFT FLATTENED PARENTHESIS
  // 0x27ef: MATHEMATICAL RIGHT FLATTENED PARENTHESIS
  // 0x2983: LEFT WHITE CURLY BRACKET
  // 0x2984: RIGHT WHITE CURLY BRACKET
  // 0x2985: LEFT WHITE PARENTHESIS
  // 0x2986: RIGHT WHITE PARENTHESIS
  // 0x2987: Z NOTATION LEFT IMAGE BRACKET
  // 0x2988: Z NOTATION RIGHT IMAGE BRACKET
  // 0x2989: Z NOTATION LEFT BINDING BRACKET
  // 0x298a: Z NOTATION RIGHT BINDING BRACKET
  // 0x298b: LEFT SQUARE BRACKET WITH UNDERBAR
  // 0x298c: RIGHT SQUARE BRACKET WITH UNDERBAR
  // 0x298d: LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
  // 0x298e: RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
  // 0x298f: LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
  // 0x2990: RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
  // 0x2991: LEFT ANGLE BRACKET WITH DOT
  // 0x2992: RIGHT ANGLE BRACKET WITH DOT
  // 0x2993: LEFT ARC LESS-THAN BRACKET
  // 0x2994: RIGHT ARC GREATER-THAN BRACKET
  // 0x2995: DOUBLE LEFT ARC GREATER-THAN BRACKET
  // 0x2996: DOUBLE RIGHT ARC LESS-THAN BRACKET
  // 0x2997: LEFT BLACK TORTOISE SHELL BRACKET
  // 0x2998: RIGHT BLACK TORTOISE SHELL BRACKET
  // 0x29d8: LEFT WIGGLY FENCE
  // 0x29d9: RIGHT WIGGLY FENCE
  // 0x29da: LEFT DOUBLE WIGGLY FENCE
  // 0x29db: RIGHT DOUBLE WIGGLY FENCE
  // 0x29fc: LEFT-POINTING CURVED ANGLE BRACKET
  // 0x29fd: RIGHT-POINTING CURVED ANGLE BRACKET
  // 0x2e02: LEFT SUBSTITUTION BRACKET
  // 0x2e03: RIGHT SUBSTITUTION BRACKET
  // 0x2e04: LEFT DOTTED SUBSTITUTION BRACKET
  // 0x2e05: RIGHT DOTTED SUBSTITUTION BRACKET
  // 0x2e09: LEFT TRANSPOSITION BRACKET
  // 0x2e0a: RIGHT TRANSPOSITION BRACKET
  // 0x2e0c: LEFT RAISED OMISSION BRACKET
  // 0x2e0d: RIGHT RAISED OMISSION BRACKET
  // 0x2e1c: LEFT LOW PARAPHRASE BRACKET
  // 0x2e1d: RIGHT LOW PARAPHRASE BRACKET
  // 0x2e20: LEFT VERTICAL BAR WITH QUILL
  // 0x2e21: RIGHT VERTICAL BAR WITH QUILL
  // 0x2e22: TOP LEFT HALF BRACKET
  // 0x2e23: TOP RIGHT HALF BRACKET
  // 0x2e24: BOTTOM LEFT HALF BRACKET
  // 0x2e25: BOTTOM RIGHT HALF BRACKET
  // 0x2e26: LEFT SIDEWAYS U BRACKET
  // 0x2e27: RIGHT SIDEWAYS U BRACKET
  // 0x2e28: LEFT DOUBLE PARENTHESIS
  // 0x2e29: RIGHT DOUBLE PARENTHESIS
  // 0x3008: LEFT ANGLE BRACKET
  // 0x3009: RIGHT ANGLE BRACKET
  // 0x300a: LEFT DOUBLE ANGLE BRACKET
  // 0x300b: RIGHT DOUBLE ANGLE BRACKET
  // 0x300c: LEFT CORNER BRACKET
  // 0x300d: RIGHT CORNER BRACKET
  // 0x300e: LEFT WHITE CORNER BRACKET
  // 0x300f: RIGHT WHITE CORNER BRACKET
  // 0x3010: LEFT BLACK LENTICULAR BRACKET
  // 0x3011: RIGHT BLACK LENTICULAR BRACKET
  // 0x3014: LEFT TORTOISE SHELL BRACKET
  // 0x3015: RIGHT TORTOISE SHELL BRACKET
  // 0x3016: LEFT WHITE LENTICULAR BRACKET
  // 0x3017: RIGHT WHITE LENTICULAR BRACKET
  // 0x3018: LEFT WHITE TORTOISE SHELL BRACKET
  // 0x3019: RIGHT WHITE TORTOISE SHELL BRACKET
  // 0x301a: LEFT WHITE SQUARE BRACKET
  // 0x301b: RIGHT WHITE SQUARE BRACKET
  // 0x301d: REVERSED DOUBLE PRIME QUOTATION MARK
  // 0x301e: DOUBLE PRIME QUOTATION MARK
  // 0x301f: LOW DOUBLE PRIME QUOTATION MARK
  // 0xfd3e: ORNATE LEFT PARENTHESIS
  // 0xfd3f: ORNATE RIGHT PARENTHESIS
  // 0xfe17: PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
  // 0xfe18: PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
  // 0xfe35: PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
  // 0xfe36: PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
  // 0xfe37: PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
  // 0xfe38: PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
  // 0xfe39: PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
  // 0xfe3a: PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
  // 0xfe3b: PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
  // 0xfe3c: PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
  // 0xfe3d: PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
  // 0xfe3e: PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
  // 0xfe3f: PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
  // 0xfe40: PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
  // 0xfe41: PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
  // 0xfe42: PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
  // 0xfe43: PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
  // 0xfe44: PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
  // 0xfe47: PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
  // 0xfe48: PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
  // 0xfe59: SMALL LEFT PARENTHESIS
  // 0xfe5a: SMALL RIGHT PARENTHESIS
  // 0xfe5b: SMALL LEFT CURLY BRACKET
  // 0xfe5c: SMALL RIGHT CURLY BRACKET
  // 0xfe5d: SMALL LEFT TORTOISE SHELL BRACKET
  // 0xfe5e: SMALL RIGHT TORTOISE SHELL BRACKET
  // 0xff08: FULLWIDTH LEFT PARENTHESIS
  // 0xff09: FULLWIDTH RIGHT PARENTHESIS
  // 0xff3b: FULLWIDTH LEFT SQUARE BRACKET
  // 0xff3d: FULLWIDTH RIGHT SQUARE BRACKET
  // 0xff5b: FULLWIDTH LEFT CURLY BRACKET
  // 0xff5d: FULLWIDTH RIGHT CURLY BRACKET
  // 0xff5f: FULLWIDTH LEFT WHITE PARENTHESIS
  // 0xff60: FULLWIDTH RIGHT WHITE PARENTHESIS
  // 0xff62: HALFWIDTH LEFT CORNER BRACKET
  // 0xff63: HALFWIDTH RIGHT CORNER BRACKET
  val delimPairs: Map[Char,Char] =
    Map(
      '(' -> ')',
      '[' -> ']',
      '{' -> '}')
  val openers = delimPairs.keySet
  val closers = delimPairs.values.toSet
  val alphaCharTypes: Set[Byte] = {
    import Character._
    Set(
      UPPERCASE_LETTER,LOWERCASE_LETTER,TITLECASE_LETTER,MODIFIER_LETTER,
      OTHER_LETTER,COMBINING_SPACING_MARK,MODIFIER_SYMBOL,NON_SPACING_MARK)
  }
  val punctCharTypes: Set[Byte] = {
    import Character._
    Set(
      CURRENCY_SYMBOL,DASH_PUNCTUATION,MATH_SYMBOL,OTHER_PUNCTUATION,
      OTHER_SYMBOL)
  }
  def charType (ch: Char): Byte = Character.getType(ch).toByte

  // Tokens and related stuff //////////////////////////////////////////////////
  trait Source
  case class Position (pos: Int) extends Source { // TODO: file name, line #
    override def toString: String = s"at position $pos"
  }
  case class Synthesized (from: Source) extends Source {
    override def toString: String = s"synthesized $from"
  }
  trait Tokenoid {
    def text: String
    def at: Source
    override def toString: String = s"$text $at"
  }
  trait Token extends Tokenoid    // raw token from input
  trait Sign extends Tokenoid     // not a delimiter, but might be Expr
  trait OpToken extends Tokenoid  // ready to be turned into an Op
  case class Open (text: String, at: Source) extends Token
  case class Close (text: String, at: Source) extends Token
  case class Sym (text: String, at: Source) extends
    Token with Sign with OpToken
  case class White (text: String, at: Source) extends Token with Sign
  case class ExprSign (expr: Expr, at: Source) extends Sign with OpToken {
    def text = s"(expression $expr)"
  }
  def start = Open("",Position(0))
  def end (at: Source) = Close("",at)
  def matches (close: Close, open: Open) =
    close.text.isEmpty && open.text.isEmpty ||
      delimPairs(open.text.head) == close.text.last 

  // Tokenizing ////////////////////////////////////////////////////////////////
  // TODO: make ;; the comment-to-end-of-line token
  // TODO: string constants and other magic
  object Chars {
    import Character._
    def isCarriageReturn (ch: Char) = ch == '\r'
    def isWhite (ch: Char) = isWhitespace(ch)
    def isAlpha (ch: Char) = alphaCharTypes.contains(charType(ch))
    def isDigit (ch: Char) = Character.isDigit(ch)
    def isConnector (ch: Char) = charType(ch) == CONNECTOR_PUNCTUATION
    def isOpen (ch: Char) = openers.contains(ch)
    def isStickyOpen (ch: Char) = ch == '['
    def isClose (ch: Char) = closers.contains(ch)
    def isPunctuation (ch: Char) = punctCharTypes.contains(charType(ch))
    def isLegal (ch: Char) =
      isWhite(ch) || isAlpha(ch) || isDigit(ch) || isConnector(ch) ||
        isOpen(ch) || isClose(ch) || isPunctuation(ch)
  }
  def tokens (in: Stream[Char]): Stream[Token] = {
    import Chars._
    def sticks (prev: Char, next: Char) =
      if (isOpen(prev) || isOpen(next) && ! isStickyOpen(next) ||
          isClose(prev) || isClose(next))
        false
      else if (isWhite(prev))
        isWhite(next)
      else if (isAlpha(prev) || isDigit(prev))
        isAlpha(next) || isDigit(next) || isConnector(next) ||
          isStickyOpen(next)
      else if (isConnector(prev))
        isAlpha(next) || isDigit(next) || isConnector(next) ||
          isStickyOpen(next) || isPunctuation(next)
      else if (isPunctuation(prev))
        isConnector(next) || isPunctuation(next)
      else
        false
    def mkToken (text: String, pos: Int): Token = {
      val at = Position(pos)
      if (isWhite(text.head)) White(text,at)
      else if (isOpen(text.last)) Open(text,at)
      else if (isClose(text.head)) Close(text,at)
      else Sym(text,at)
    }
    @tailrec def next (
          tokenSoFar: String, tokenStart: Int, in: Stream[Char], pos: Int):
        (Token,Stream[Char],Int) = {
      in.headOption match {
        case None =>
          if (tokenSoFar.isEmpty) (end(Position(pos)),in,pos)
          else (mkToken(tokenSoFar,tokenStart),in,pos)
        case Some(ch) =>
          if (! isLegal(ch))
            throw new IllegalArgumentException(
              "Illegal character code " + ch.toInt + " at position " + pos)
          else if (tokenSoFar.isEmpty || sticks(tokenSoFar.last,ch))
            next(tokenSoFar :+ ch,tokenStart,in.tail,pos + 1)
          else
            (mkToken(tokenSoFar,tokenStart),in,pos)
      }
    }
    Stream.iterate((start: Token,in,0)) { case (token,chars,pos) =>
      next("",pos,chars,pos)
    }.map(_._1)
  }

  // Kinds of macros ///////////////////////////////////////////////////////////
  trait DelimMacro {
    def openName: String
    def f: List[Sign] => Expr
  }
  trait OrdinaryMacro {
    def name: String
    def f: (Expr,Expr) => Expr  // TODO: no reason all macros should be binary
  }

  // Standard delimiter macros /////////////////////////////////////////////////
  val binaryPrecedences: Map[Char,Int] =
      Seq(
          ";",
          "~",
          "|",
          ",",
          ":",
          "?",
          "!",
          "<=>",
          "#$&",
          "+-",
          "*/%",
          "^",
          "@",
          ".").
        zipWithIndex.flatMap { case (s,i) => s.map(ch => ch -> i) }.toMap
  val rightAssociativeEndings: Set[Char] = Set(':',',',';')
  def isUnary (s: Sign) = {
    import Chars._
    s match {
      case Sym(text,_) if ! isPunctuation(text.head) => true
      case ExprSign(_,_) => true
      case _ => false
    }
  }
  def isBinary (s: Sign) = s match {
    case sym: Sym if ! isUnary(sym) => true
    case _ => false
  }
  trait Op {
    def sign: Sign
    def asExpr =
      sign match {
        case ExprSign(e,_) => e
        case Sym(text,_) => Var(text)
      }
  }
  /** Unary operators are prefix, left-associative, and all same precedence. */
  case class Unary (sign: Sign) extends Op {
    override def toString: String = s"1<$sign>"
  }
  case class Precedence (tight: Boolean, prec: Int) extends
      Ordered[Precedence] {
    def compare (that: Precedence) =
      if (this.tight && ! that.tight) 1
      else if (that.tight && ! this.tight) -1
      else this.prec - that.prec
  }
  /** Tight binary operators have higher precedence than unary; loose, lower. */
  case class Binary (sign: Sign, tight: Boolean) extends Op {
    lazy val precedence = Precedence(tight,binaryPrecedences(sign.text.head))
    lazy val rightAssoc = rightAssociativeEndings.contains(sign.text.last)
    override def toString: String = s"2<$sign>"
  }
  def loose (sign: Sign) = Binary(sign,false)
  def tight (sign: Sign) = Binary(sign,true)
  @tailrec def ops (
        semi: Boolean, signs: List[Sign], revAcc: List[Op], free: Set[String]):
      (List[Op],Set[String]) =
    signs match {
      case Nil =>
        (revAcc,Set.empty)
      case White(_,_) :: b :: tail if isBinary(b) =>
        ops(semi,tail,loose(b) :: revAcc,free + b.text)
      case b :: White(_,_) :: tail if isBinary(b) =>
        ops(semi,tail,loose(b) :: revAcc,free + b.text)
      case b :: tail if isBinary(b) =>
        ops(semi,tail,tight(b) :: revAcc,free + b.text)
      case White(text,at) :: (tail @ (u :: _)) if
          semi && text.contains('\n') && isUnary(u) =>
        revAcc.headOption match {
          case Some(Unary(_)) =>
            ops(semi,tail,loose(Sym(";",Synthesized(at))) :: revAcc,free + ";")
          case _ =>
            ops(semi,tail,revAcc,free)
        }
      case White(_,_) :: tail =>
        ops(semi,tail,revAcc,free)
      case (es @ ExprSign(e,_)) :: tail =>
        ops(semi,tail,Unary(es) :: revAcc,free ++ freeVarNames(e))
      case (u @ Sym(text,_)) :: tail if isUnary(u) =>
        ops(semi,tail,Unary(u) :: revAcc,free + text)
      case _ =>
        sys.error(s"Don't know what to do with $signs after ${revAcc.reverse}")
    }
  def rev (revOps: List[Op], freeVars: Set[String]): (List[Op],Expr => Expr) = {
    def rev2 (
          os: List[Op], acc: List[Op], f: Expr => Expr, vs: Stream[String]):
        (List[Op],Expr => Expr) = os match {
      case Nil =>
        (acc,f)
      case (b: Binary) :: _ if acc.isEmpty =>
        var varName = vs.head
        rev2(
          os,List(Unary(Sym(varName,Synthesized(b.sign.at)))),
          e => OLm(varName,f(e)),vs.tail)
      case (b: Binary) :: tail if
          (tail match {
            case (_: Binary) :: _ => true
            case Nil => true
            case _ => false
          }) =>
        var varName = vs.head
        rev2(
          tail,Unary(Sym(varName,Synthesized(b.sign.at))) :: b :: acc,
          e => OLm(varName,f(e)),vs.tail)
      case o :: tail =>
        rev2(tail,o :: acc,f,vs)
    }
    rev2(revOps,Nil,(e: Expr) => e,freshNames(freeVars))
  }
  def binary (
        ops: List[Op], from: Precedence, acc: List[(Expr,Binary)]):
      (Expr,List[Op]) = {
    val (arg,remOps) = if (from.tight) unary(ops) else unaries(ops)
    def accPrec = acc.head._2.precedence
    def mkExpr (arg: Expr): Expr =
      if (acc.isEmpty)
        arg
      else {
        val rAcc = acc.reverse // to put into original order
        assert(rAcc.groupBy(_._2.precedence).size == 1) // all same precedence
        val (rightAssocs,leftAssocs) = rAcc.map(_._2).partition(_.rightAssoc)
        if (! (rightAssocs.isEmpty || leftAssocs.isEmpty))
          sys.error(
            "In the same expression, these operators are left-associative:\n" +
              leftAssocs.mkString("\n","\n","\n") +
              "\nbut these are right-associative:\n" +
              rightAssocs.mkString("\n","\n","\n"))
        def app (op: Binary, e1: Expr, e2: Expr) = App(App(op.asExpr,e1),e2)
        if (leftAssocs.isEmpty)
          rAcc.foldRight(arg) { case ((e1,op),e2) => app(op,e1,e2) }
        else {
          val (args,binOps) = rAcc.unzip
          val (arg0,rAcc2) = (args.head,binOps.zip(args.tail :+ arg))
          rAcc2.foldLeft(arg0) { case (e1,(op,e2)) => app(op,e1,e2) }
        }
      }
    def binary2 (arg: Expr, remOps: List[Op]): (Expr,List[Op]) =
      remOps.headOption match {
        case None | Some(Unary(_)) =>
          (mkExpr(arg),remOps)
        case Some(b: Binary) if b.precedence <= from =>
          (mkExpr(arg),remOps)
        case Some(b: Binary) if acc.isEmpty || b.precedence == accPrec =>
          binary(remOps.tail,from,(arg,b) :: acc)
        case Some(b: Binary) if b.precedence < accPrec =>
          binary(remOps.tail,from,List((mkExpr(arg),b)))
        case Some(b: Binary) if b.precedence > accPrec =>
          val (arg2,remOps2) = binary(remOps.tail,accPrec,List((arg,b)))
          binary2(arg2,remOps2)
      }
    binary2(arg,remOps)
  }
  def unary (ops: List[Op]): (Expr,List[Op]) = ops match {
      case (u: Unary) :: (b: Binary) :: tail if b.tight =>
        binary(tail,Precedence(true,-1),List((u.asExpr,b)))
      case (u: Unary) :: tail =>
        (u.asExpr,tail)
  }
  def unaries (ops: List[Op]): (Expr,List[Op]) = {
    val (e,remOps) = unary(ops)
    def unaries2 (e: Expr, remOps: List[Op]): (Expr,List[Op]) = remOps match {
      case Unary(_) :: _ =>
        val (e2,remOps2) = unary(remOps)
        unaries2(App(e,e2),remOps2)
      case _ =>
        (e,remOps)
    }
    unaries2(e,remOps)
  }
  def standardDelim (inferSemicolons: Boolean) (signs: List[Sign]): Expr = {
    val (os,free) = ops(inferSemicolons,signs,Nil,Set.empty)
    if (os.isEmpty)
      Lit((),UUnit)
    else {
      val (os2,lambdaWrapper) = rev(os,free)
      val (e,remOps) = binary(os2,Precedence(false,-1),Nil)
      if (! remOps.isEmpty)
        sys.error(s"Leftovers at end of parse: $remOps")
      etaConvert(lambdaWrapper(e))
    }
  }

  // Parsing ///////////////////////////////////////////////////////////////////
  def parse (in: Stream[Char]): Expr = {
    def next (
          open: Open, ts: Stream[Token], revAcc: List[Sign]):
        (Expr,Stream[Token]) =
      ts.head match {
        case open1 @ Open(_,at) =>
          val (e,ts1) = next(open1,ts.tail,Nil)
          next(open,ts1,ExprSign(e,at) :: revAcc)
        case close @ Close(_,_) =>
          if (matches(close,open))
            delimMacros.get(open.text) match {
              case None => sys.error(s"Undefined delimiter macro ${open.text}")
              case Some(m) => (m.f(revAcc.reverse),ts.tail)
            }
          else
            sys.error(s"Closing delimiter $close doesn't match $open")
        case sym @ Sym(_,_) =>
          next(open,ts.tail,sym :: revAcc)
        case white @ White(_,_) =>
          next(open,ts.tail,white :: revAcc)
      }
    next(start,tokens(in),Nil)._1
  }
    
  // Built-in macros ///////////////////////////////////////////////////////////
  case class DM (openName: String, f: List[Sign] => Expr) extends DelimMacro
  case class OM (name: String, f: (Expr,Expr) => Expr) extends OrdinaryMacro
  val delimMacros: Map[String,DelimMacro] =
    List(
      DM("",standardDelim(true)),
      DM("{",standardDelim(true)),
      DM("(",standardDelim(false))).map(m => m.openName -> m).toMap
  val ordinaryMacros: Map[String,OrdinaryMacro] =
    List(
      OM(";",semicolon _),
      OM("~",tilde _),
      OM("~~",doubleTilde _),
      OM(",",comma _),
      OM("-:",dashColon _),
      OM(":",colon _),
      OM("??",doubleQuestionMark _),
      OM("!!",doubleBang _)).map(m => m.name -> m).toMap
  def semicolon (e1: Expr, e2: Expr): Expr =
    e1 match {
      case App(App(Var("~~"),Var(varName)),defn) =>
        App(OLm(varName,e2),defn)
      case App(App(Var("~~"),App(App(Var(":"),Var(varName)),varType)),defn) =>
        App(Lam(varName,varType,e2),defn)
      case App(App(Var("~"),Var(varName)),defn) =>
        App(OLm(varName,e2),Fix(varName,Om,defn))
      case App(App(Var("~"),App(App(Var(":"),Var(varName)),varType)),defn) =>
        App(Lam(varName,varType,e2),Fix(varName,varType,defn))
      case _ =>                 // e1 is executed only for its side effects
        App(OLm(freshVarName(e2),e2),e1)
    }
  def binding (bindOp: String, e1: Expr, e2: Expr): Expr =
    e1 match {
      case v @ Var(_) =>
        semicolon(App(App(Var(bindOp),v),e2),Lit((),UUnit))
      case vt @ App(App(Var(":"),Var(_)),_) =>
        semicolon(App(App(Var(bindOp),vt),e2),Lit((),UUnit))
      case _ =>
        sys.error(s"Left hand of $bindOp should be var or (var: type): $e1")
    }
  def tilde (e1: Expr, e2: Expr): Expr = binding("~",e1,e2)
  def doubleTilde (e1: Expr, e2: Expr): Expr = binding("~~",e1,e2)
  def comma (e1: Expr, e2: Expr): Expr =
    e1 match {
      case Var(varName) => OLm(varName,e2)
      case App(App(Var(":"),Var(varName)),varType) => Lam(varName,varType,e2)
      case _ => sys.error(s"Can't apply , to $e1 and $e2")
    }
  def dashColon (e1: Expr, e2: Expr): Expr =
    e1 match {
      case App(App(Var(":"),Var(varName)),from) => DFT(varName,from,e2)
      case _ => NFT(e1,e2)
    }
  def colon (e1: Expr, e2: Expr): Expr = Cast(e1,e2)
  def doubleQuestionMark (e1: Expr, e2: Expr): Expr =
    e2 match {
      case App(App(Var("!!"),trueBranch),falseBranch) =>
        If(e1,trueBranch,falseBranch)
      case _ =>
        sys.error("?? expects a !! for its right argument")
    }
  def doubleBang (e1: Expr, e2: Expr): Expr =
    sys.error("!! should always appear as part of ??")

  // Macro preprocessing ///////////////////////////////////////////////////////
  def expand (e: Expr): Expr = e match {
    case App(App(Var(m),e1),e2) if ordinaryMacros.contains(m) =>
      expand(ordinaryMacros(m).f(e1,e2))
    case DFT(varName,from,to) =>
      DFT(varName,expand(from),expand(to))
    case App(fun,arg) =>
      App(expand(fun),expand(arg))
    case Lam(varName,varType,body) =>
      Lam(varName,expand(varType),expand(body))
    case If(cond,trueBranch,falseBranch) =>
      If(expand(cond),expand(trueBranch),expand(falseBranch))
    case Cast(term,toType) =>
      Cast(expand(term),expand(toType))
    case Fix(varName,varType,body) =>
      Fix(varName,expand(varType),expand(body))
    case _ =>
      e
  }

  // The whole shebang /////////////////////////////////////////////////////////
  def compile (chars: Stream[Char]): Expr = expand(parse(chars))
  def compile (s: String): Expr = compile(s.toStream)

}
