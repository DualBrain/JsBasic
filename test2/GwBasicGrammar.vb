Imports Irony.Parsing

''' <summary>
''' This class defines the Grammar for the GwBASIC language.
''' (Not complete.)
''' </summary>
''' <remarks>
''' http://www.xs4all.nl/~hwiegman/gw-man/index.html
''' or
''' http://www.geocities.com/KindlyRat/GWBASIC_Help.zip
''' </remarks>
<Language("GwBasic", "1.01", "Sample GW Basic grammar")>
Public Class GWBasicGrammar
  Inherits Grammar

  Public Sub New()
    MyBase.New(False)  ' BASIC is not case sensitive...

    Me.GrammarComments = "This grammar uses one new Irony feature - Scanner-Parser link. Parser helps Scanner to disambiguate " &
                         "similar but different token types when more than one terminal matches the current input char." & vbCrLf &
                         "See comments in GwBasicGrammar.cs file."
    ' 
    ' Scanner-Parser link. 
    ' The grammar defines 3 terminals for numbers: number, lineNumber and fileNumber. All three return decimal 
    ' digits in GetFirsts() method. So when current input is a digit, the scanner has 3 potential candidate terminals
    ' to match the input. However, because each of the 3 terminals can appear in specific "places" in grammar, 
    ' the parser is able to assist scanner to pick the correct terminal, depending on the current parser state. 
    ' The disambiguation happens in Scanner.SelectTerminals method. When the terminal list for current input char
    ' has more than 1 terminal, the scanner code gets the current parser state from core parser (through compilerContext), 
    ' and then checks the terminals agains the ExpectedTerms set in the parser state. 
    ' As you might see in Grammar Explorer, the conflict is resolved successfully. 
    ' 

    'Terminals
    Dim lineNumber As New NumberLiteral("LINE_NUMBER",
                                        NumberOptions.IntOnly)
    Dim fileNumber As New NumberLiteral("FILE_NUMBER",
                                        NumberOptions.IntOnly)

    'ints that are too long for int32 are converted to int64
    Dim number = New NumberLiteral("NUMBER",
                                   NumberOptions.AllowStartEndDot) With {.DefaultIntTypes = New TypeCode() {TypeCode.Int32,
                                                                                                                   TypeCode.Int64}}
    number.AddExponentSymbols("eE", TypeCode.[Single])
    number.AddExponentSymbols("dD", TypeCode.[Double])
    number.AddSuffix("!", TypeCode.[Single])
    number.AddSuffix("#", TypeCode.[Double])

    Dim variable As New IdentifierTerminal("Identifier")
    variable.AddSuffix("$", TypeCode.[String])
    variable.AddSuffix("%", TypeCode.Int32)
    variable.AddSuffix("!", TypeCode.[Single])
    variable.AddSuffix("#", TypeCode.[Double])

    Dim stringLiteral1 As New StringLiteral("STRING",
                                            """",
                                            StringOptions.None)
    'Important: do not add comment term to base.NonGrammarTerminals list - we do use this terminal in grammar rules
    Dim userFunctionName = variable
    Dim comment As New CommentTerminal("Comment",
                                       "REM",
                                       Microsoft.VisualBasic.vbLf)
    Dim short_comment As New CommentTerminal("ShortComment",
                                             "'",
                                             Microsoft.VisualBasic.vbLf)
    Dim line_cont As New LineContinuationTerminal("LineContinuation",
                                                  "_")
    Dim comma = ToTerm(",", "comma")
    Dim colon = ToTerm(":", "colon")

    Dim comma_opt = New NonTerminal("comma_opt") With {.Rule = Empty Or ","}
    Dim semi_opt = New NonTerminal("semi_opt") With {.Rule = Empty Or ";"}
    Dim pound_opt = New NonTerminal("pound_opt") With {.Rule = Empty Or "#"}

    ' Non-terminals
    Dim PROGRAM As New NonTerminal("PROGRAM")
    Dim LINE As New NonTerminal("LINE")
    Dim LINE_CONTENT_OPT As New NonTerminal("LINE_CONTENT_OPT")
    Dim COMMENT_OPT As New NonTerminal("COMMENT_OPT")
    Dim STATEMENT_LIST As New NonTerminal("STATEMENT_LIST")
    Dim STATEMENT As New NonTerminal("STATEMENT")
    Dim PRINT_STMT As New NonTerminal("PRINT_STMT")
    Dim PRINT_LIST As New NonTerminal("PRINT_LIST")
    Dim PRINT_ARG As New NonTerminal("PRINT_ARG")
    Dim OPEN_STMT As New NonTerminal("OPEN_STMT")
    Dim OPEN_STMT_MODE As New NonTerminal("OPEN_STMT_MODE")
    Dim OPEN_STMT_ACCESS As New NonTerminal("OPEN_STMT_ACCESS")
    Dim CLOSE_STMT As New NonTerminal("CLOSE_STMT")
    Dim INPUT_STMT As New NonTerminal("INPUT_STMT")
    Dim VARIABLES As New NonTerminal("VARIABLES")
    Dim IF_STMT As New NonTerminal("IF_STMT")
    Dim THEN_CLAUSE As New NonTerminal("THEN_CLAUSE")
    Dim ELSE_CLAUSE_OPT As New NonTerminal("ELSE_CLAUSE_OPT") ', typeof(AstNode));
    Dim EXPR As New NonTerminal("EXPRESSION")
    Dim EXPR_LIST As New NonTerminal("EXPRESSION_LIST")
    Dim BINARY_OP As New NonTerminal("BINARY_OP", "operator")
    Dim BINARY_EXPR As New NonTerminal("BINARY_EXPR")
    Dim UNARY_EXPR As New NonTerminal("UNARY_EXPR")
    Dim SIGN As New NonTerminal("SIGN")
    Dim ASSIGN_STMT As New NonTerminal("ASSIGN_STMT")
    Dim FOR_STMT As New NonTerminal("FOR_STMT")
    Dim STEP_OPT As New NonTerminal("STEP_OPT")
    Dim NEXT_STMT As New NonTerminal("NEXT_STMT")
    Dim LOCATE_STMT As New NonTerminal("LOCATE_STMT")
    Dim WHILE_STMT As New NonTerminal("WHILE_STMT")
    Dim WEND_STMT As New NonTerminal("WEND_STMT")
    Dim SWAP_STMT As New NonTerminal("SWAP_STMT")
    Dim FUN_CALL As New NonTerminal("FUN_CALL")
    Dim VARIABLE_OR_FUNCTION_EXPR As New NonTerminal("VARIABLE_OR_FUNCTION_EXPR")
    Dim ARG_LIST As New NonTerminal("ARG_LIST")
    Dim LINE_INPUT_STMT As New NonTerminal("LINE_INPUT_STMT")
    Dim LINE_INPUT_POUND_STMT As New NonTerminal("LINE_INPUT_POUND_STMT")
    Dim END_STMT As New NonTerminal("END_STMT")
    Dim CLS_STMT As New NonTerminal("CLS_STMT")
    Dim CLEAR_STMT As New NonTerminal("CLEAR_STMT")
    Dim DIM_STMT As New NonTerminal("DIM_STMT")
    Dim DEF_FN_STMT As New NonTerminal("DEF_FN_STMT")
    Dim GOTO_STMT As New NonTerminal("GOTO_STMT")
    Dim GOSUB_STMT As New NonTerminal("GOSUB_STMT")
    Dim RETURN_STMT As New NonTerminal("RETURN_STMT")
    Dim ON_STMT As New NonTerminal("ON_STMT")
    Dim LINE_NUMBERS As New NonTerminal("LINE_NUMBERS")
    Dim RANDOMIZE_STMT As New NonTerminal("RANDOMIZE_STMT")

    ' set the PROGRAM to be the root node of BASIC programs.
    Me.Root = PROGRAM

    ' BNF Rules
    PROGRAM.Rule = MakePlusRule(PROGRAM, LINE)

    ' A line can be an empty line, or it's a number followed by a statement list ended by a new-line.
    LINE.Rule = NewLine Or
                lineNumber + LINE_CONTENT_OPT + COMMENT_OPT + NewLine Or
                SyntaxError + NewLine
    LINE.NodeCaptionTemplate = "Line #{0}"

    ' A statement list is 1 or more statements separated by the ':' character
    LINE_CONTENT_OPT.Rule = Empty Or
                            IF_STMT Or
                            STATEMENT_LIST
    STATEMENT_LIST.Rule = MakePlusRule(STATEMENT_LIST,
                                       colon,
                                       STATEMENT)
    COMMENT_OPT.Rule = short_comment Or
                       comment Or
                       Empty

    ' A statement can be one of a number of types
    STATEMENT.Rule = ASSIGN_STMT Or
                     PRINT_STMT Or
                     INPUT_STMT Or
                     OPEN_STMT Or
                     CLOSE_STMT Or
                     LINE_INPUT_POUND_STMT Or
                     LINE_INPUT_STMT Or
                     LOCATE_STMT Or
                     CLS_STMT Or
                     END_STMT Or
                     CLEAR_STMT Or
                     DIM_STMT Or
                     DEF_FN_STMT Or
                     SWAP_STMT Or
                     RANDOMIZE_STMT Or
                     GOSUB_STMT Or
                     RETURN_STMT Or
                     GOTO_STMT Or
                     ON_STMT Or
                     FOR_STMT Or
                     NEXT_STMT Or
                     WHILE_STMT Or
                     WEND_STMT

    ' The different statements are defined here
    PRINT_STMT.Rule = "print" + PRINT_LIST
    PRINT_LIST.Rule = MakeStarRule(PRINT_LIST,
                                   Nothing,
                                   PRINT_ARG)
    PRINT_ARG.Rule = EXPR + semi_opt
    INPUT_STMT.Rule = "input" + semi_opt + stringLiteral1 + ";" + VARIABLES
    OPEN_STMT.Rule = "open" + EXPR + (Empty Or "for" + OPEN_STMT_MODE) + (Empty Or "access" + OPEN_STMT_ACCESS) + "as" + pound_opt + fileNumber
    OPEN_STMT_ACCESS.Rule = "read" + (Empty Or "write") Or
                            "write"
    OPEN_STMT_MODE.Rule = ToTerm("o") Or
                          "i" Or
                          "a" Or
                          "output" Or
                          "input" Or
                          "append"
    CLOSE_STMT.Rule = "close" + pound_opt + fileNumber
    LINE_INPUT_STMT.Rule = ToTerm("line") + "input" + semi_opt + stringLiteral1 + ";" + VARIABLE_OR_FUNCTION_EXPR
    LINE_INPUT_POUND_STMT.Rule = ToTerm("line") + "input" + ToTerm("#") + fileNumber + comma + VARIABLE_OR_FUNCTION_EXPR
    DIM_STMT.Rule = "dim" + VARIABLES
    DEF_FN_STMT.Rule = "def" + userFunctionName + (Empty Or "(" + ARG_LIST + ")") + "=" + EXPR
    VARIABLES.Rule = VARIABLE_OR_FUNCTION_EXPR Or
                     VARIABLE_OR_FUNCTION_EXPR + "," + VARIABLES

    IF_STMT.Rule = "if" + EXPR + THEN_CLAUSE + ELSE_CLAUSE_OPT
    THEN_CLAUSE.Rule = "then" + STATEMENT_LIST Or
                       GOTO_STMT

    'Inject PreferShift hint here to explicitly set shift as preferred action. Suppresses warning message about conflict. 
    ELSE_CLAUSE_OPT.Rule = Empty Or
                           PreferShiftHere() + "else" + STATEMENT_LIST

    GOTO_STMT.Rule = "goto" + lineNumber
    GOSUB_STMT.Rule = "gosub" + lineNumber
    RETURN_STMT.Rule = "return"
    ON_STMT.Rule = "on" + EXPR + (ToTerm("goto") Or "gosub") + LINE_NUMBERS
    LINE_NUMBERS.Rule = MakePlusRule(LINE_NUMBERS,
                                     comma,
                                     lineNumber)
    ASSIGN_STMT.Rule = VARIABLE_OR_FUNCTION_EXPR + "=" + EXPR
    LOCATE_STMT.Rule = "locate" + EXPR + comma + EXPR
    SWAP_STMT.Rule = "swap" + EXPR + comma + EXPR
    END_STMT.Rule = "end"
    CLS_STMT.Rule = "cls"
    CLEAR_STMT.Rule = ToTerm("clear") + comma + (Empty Or number) + (Empty Or comma + number) Or
                      "clear" + number Or
                      "clear"
    RANDOMIZE_STMT.Rule = "randomize" + EXPR

    ' An expression is a number, or a variable, a string, or the result of a binary comparison.
    EXPR.Rule = number Or
                variable Or
                FUN_CALL Or
                stringLiteral1 Or
                BINARY_EXPR Or
                "(" + EXPR + ")" Or
                UNARY_EXPR
    BINARY_EXPR.Rule = EXPR + BINARY_OP + EXPR
    UNARY_EXPR.Rule = SIGN + EXPR
    SIGN.Rule = ToTerm("-") Or
                "+"

    'Inject PreferShift hint here to explicitly set shift as preferred action. Suppresses warning message about conflict. 
    'The conflict arises from PRINT statement, when there may be ambigous interpretations for expression like
    '  PRINT F (X) -- either function call or identifier followed by expression
    FUN_CALL.Rule = variable + PreferShiftHere() + "(" + ARG_LIST + ")"
    VARIABLE_OR_FUNCTION_EXPR.Rule = variable Or
                                     FUN_CALL

    BINARY_OP.Rule = ToTerm("+") Or
                     "^" Or
                     "-" Or
                     "*" Or
                     "/" Or
                     "=" Or
                     "<=" Or
                     ">=" Or
                     "<" Or
                     ">" Or
                     "<>" Or
                     "and" Or
                     "or"

    'let's do operator precedence right here
    RegisterOperators(60, "^")
    RegisterOperators(50, "*", "/")
    RegisterOperators(40, "+", "-")
    RegisterOperators(30, "=", "<=", ">=", "<", ">", "<>")
    RegisterOperators(20, "and", "or")

    EXPR_LIST.Rule = MakeStarRule(EXPR_LIST, EXPR)

    FOR_STMT.Rule = "for" + ASSIGN_STMT + "to" + EXPR + STEP_OPT
    STEP_OPT.Rule = Empty Or
                    "step" + EXPR
    NEXT_STMT.Rule = "next" + VARIABLES Or
                     "next"
    WHILE_STMT.Rule = "while" + EXPR
    WEND_STMT.Rule = "wend"

    'TODO: check number of arguments for particular function in node constructor
    ARG_LIST.Rule = MakePlusRule(ARG_LIST,
                                 comma,
                                 EXPR)


    'Punctuation and Transient elements
    MarkPunctuation("(", ")", ",")
    MarkTransient(EXPR,
                  STATEMENT,
                  LINE_CONTENT_OPT,
                  VARIABLE_OR_FUNCTION_EXPR,
                  COMMENT_OPT)
    NonGrammarTerminals.Add(line_cont)

    LanguageFlags = LanguageFlags.NewLineBeforeEOF

    AddHandler lineNumber.ValidateToken, AddressOf LineNumber_ValidateToken

  End Sub

  Private Sub LineNumber_ValidateToken(sender As Object, e As ParsingEventArgs)
    If e.Context.CurrentToken.ValueString.Length > 4 Then
      e.Context.CurrentToken = e.Context.CreateErrorToken("Line number cannot be longer than 4 characters")
    End If
  End Sub

End Class