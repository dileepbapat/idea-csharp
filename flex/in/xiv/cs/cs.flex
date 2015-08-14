package in.xiv.cs;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static in.xiv.cs.psi.CsTokens.*;

%%

%{
  public _CsLexer() {
    this((java.io.Reader)null);
  }
%}

%public
%class _CsLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode


EOL="\r"|"\n"|"\r\n"
LINE_WS=[\ \t\f]
WHITE_SPACE=({LINE_WS}|{EOL})+
XID_START = [a-zA-Z_]
XID_CONTINUE = [a-zA-Z0-9_]
HEX_DIGIT = [a-fA-F0-9]
DEC_LIT = [0-9][0-9_]*
DOUBLE_QUOTE = \x22
SINGLE_QUOTE = \x27
STRING = {DOUBLE_QUOTE} ( [^\"\\] | "\\" ( {DOUBLE_QUOTE} | {SINGLE_QUOTE}) )* {DOUBLE_QUOTE}


%%
<YYINITIAL> {
  {WHITE_SPACE}          { return com.intellij.psi.TokenType.WHITE_SPACE; }

  "abstract"         { return KW_ABSTRACT; }
  "as"               { return KW_AS; }
  "base"             { return KW_BASE; }
  "bool"             { return KW_BOOL; }
  "break"            { return KW_BREAK; }
  "byte"             { return KW_BYTE; }
  "case"             { return KW_CASE; }
  "catch"            { return KW_CATCH; }
  "char"             { return KW_CHAR; }
  "checked"          { return KW_CHECKED; }
  "class"            { return KW_CLASS; }
  "const"            { return KW_CONST; }
  "continue"         { return KW_CONTINUE; }
  "decimal"          { return KW_DECIMAL; }
  "delegate"         { return KW_DELEGATE; }
  "do"               { return KW_DO; }
  "double"           { return KW_DOUBLE; }
  "else"             { return KW_ELSE; }
  "enum"             { return KW_ENUM; }
  "event"            { return KW_EVENT; }
  "explicit"         { return KW_EXPLICIT; }
  "extern"           { return KW_EXTERN; }
  "finally"          { return KW_FINALLY; }
  "fixed"            { return KW_FIXED; }
  "float"            { return KW_FLOAT; }
  "for"              { return KW_FOR; }
  "foreach"          { return KW_FOREACH; }
  "goto"             { return KW_GOTO; }
  "if"               { return KW_IF; }
  "implicit"         { return KW_IMPLICIT; }
  "in"               { return KW_IN; }
  "int"              { return KW_INT; }
  "interface"        { return KW_INTERFACE; }
  "internal"         { return KW_INTERNAL; }
  "is"               { return KW_IS; }
  "lock"             { return KW_LOCK; }
  "long"             { return KW_LONG; }
  "namespace"        { return KW_NAMESPACE; }
  "new"              { return KW_NEW; }
  "null"             { return KW_NULL; }
  "object"           { return KW_OBJECT; }
  "operator"         { return KW_OPERATOR; }
  "out"              { return KW_OUT; }
  "override"         { return KW_OVERRIDE; }
  "params"           { return KW_PARAMS; }
  "private"          { return KW_PRIVATE; }
  "protected"        { return KW_PROTECTED; }
  "public"           { return KW_PUBLIC; }
  "readonly"         { return KW_READONLY; }
  "ref"              { return KW_REF; }
  "return"           { return KW_RETURN; }
  "sbyte"            { return KW_SBYTE; }
  "sealed"           { return KW_SEALED; }
  "short"            { return KW_SHORT; }
  "sizeof"           { return KW_SIZEOF; }
  "stackalloc"       { return KW_STACKALLOC; }
  "static"           { return KW_STATIC; }
  "string"           { return KW_STRING; }
  "struct"           { return KW_STRUCT; }
  "switch"           { return KW_SWITCH; }
  "this"             { return KW_THIS; }
  "throw"            { return KW_THROW; }
  "try"              { return KW_TRY; }
  "typeof"           { return KW_TYPEOF; }
  "uint"             { return KW_UINT; }
  "ulong"            { return KW_ULONG; }
  "unchecked"        { return KW_UNCHECKED; }
  "unsafe"           { return KW_UNSAFE; }
  "ushort"           { return KW_USHORT; }
  "using"            { return KW_USING; }
  "virtual"          { return KW_VIRTUAL; }
  "void"             { return KW_VOID; }
  "volatile"         { return KW_VOLATILE; }
  "while"            { return KW_WHILE; }
  "add"              { return KW_LITERAL_ADD; }
  "remove"           { return KW_LITERAL_REMOVE; }
  "get"              { return KW_LITERAL_GET; }
  "set"              { return KW_LITERAL_SET; }
  "assembly"         { return KW_LITERAL_ASSEMBLY; }
  "field"            { return KW_LITERAL_FIELD; }
  "method"           { return KW_LITERAL_METHOD; }
  "module"           { return KW_LITERAL_MODULE; }
  "param"            { return KW_LITERAL_PARAM; }
  "property"         { return KW_LITERAL_PROPERTY; }
  "type"             { return KW_LITERAL_TYPE; }
  "."                { return KW_DOT; }
  "("                { return KW_OPEN_PAREN; }
  ")"                { return KW_CLOSE_PAREN; }
  "!"                { return KW_LOG_NOT; }
  "&&"               { return KW_LOG_AND; }
  "||"               { return KW_LOG_OR; }
  "=="               { return KW_EQUAL; }
  "!="               { return KW_NOT_EQUAL; }
  "{"                { return KW_OPEN_CURLY; }
  "}"                { return KW_CLOSE_CURLY; }
  "["                { return KW_OPEN_BRACK; }
  "]"                { return KW_CLOSE_BRACK; }
  ","                { return KW_COMMA; }
  ":"                { return KW_COLON; }
  ";"                { return KW_SEMICOLON; }
  "+"                { return KW_PLUS; }
  "-"                { return KW_MINUS; }
  "*"                { return KW_STAR; }
  "/"                { return KW_DIV; }
  "%"                { return KW_MOD; }
  "&"                { return KW_BIN_AND; }
  "|"                { return KW_BIN_OR; }
  "^"                { return KW_BIN_XOR; }
  "~"                { return KW_BIN_NOT; }
  "="                { return KW_ASSIGN; }
  "<"                { return KW_LTHAN; }
  ">"                { return KW_GTHAN; }
  "?"                { return KW_QUESTION; }
  "++"               { return KW_INC; }
  "--"               { return KW_DEC; }
  "<<"               { return KW_SHIFTL; }
  ">>"               { return KW_SHIFTR; }
  "<="               { return KW_LTE; }
  ">="               { return KW_GTE; }
  "+="               { return KW_PLUS_ASSIGN; }
  "-="               { return KW_MINUS_ASSIGN; }
  "*="               { return KW_STAR_ASSIGN; }
  "/="               { return KW_DIV_ASSIGN; }
  "%="               { return KW_MOD_ASSIGN; }
  "&="               { return KW_BIN_AND_ASSIGN; }
  "|="               { return KW_BIN_OR_ASSIGN; }
  "^="               { return KW_BIN_XOR_ASSIGN; }
  "<<="              { return KW_SHIFTL_ASSIGN; }
  ">>="              { return KW_SHIFTR_ASSIGN; }
  "->"               { return KW_DEREF; }
  "//"               { return KW_LINE_COMMENT; }
  "false"            { return KW_FALSE; }
  "true"             { return KW_TRUE; }
  "LINE_COMMENT"     { return LINE_COMMENT; }
  "SINGLE_CHAR"      { return SINGLE_CHAR; }
  {EOL}              { return NEW_LINE; }
  {XID_START}{XID_CONTINUE}*           { return IDENTIFIER_OR_KEYWORD; }
//  "BIN_LIT"              { return BIN_LIT; }
//  "OCT_LIT"              { return OCT_LIT; }
//  "HEX_LIT"              { return HEX_LIT; }
  {DEC_LIT}              { return DEC_LIT; }
  {STRING}             { return STRING_LITERAL; }
  "RAW_STRING_LIT"       { return RAW_STRING_LIT; }
  "CHAR_LIT"             { return CHAR_LIT; }
  "BLOCK_COMMENT"        { return BLOCK_COMMENT; }
  "BLOCK_DOC_COMMENT"    { return BLOCK_DOC_COMMENT; }
  "LINE_COMMENT"         { return LINE_COMMENT; }
  "LINE_DOC_COMMENT"     { return LINE_DOC_COMMENT; }
  "KW_AS"                { return KW_AS; }
  "BAD_CHARACTER"        { return BAD_CHARACTER; }
  "struct_property"      { return STRUCT_PROPERTY; }
  "impl_subitem"         { return IMPL_SUBITEM; }
  "extern_function"      { return EXTERN_FUNCTION; }


  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
