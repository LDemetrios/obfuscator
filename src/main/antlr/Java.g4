// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar Java;

//=============

start_
    : file EOF
    ;

file
    : packageDeclaration? importDeclaration* (classDeclaration | ';')* EOF
    ;

classDeclaration
    : (modifier)* 'class'  identifier typeParameters? classExtends? classImplements? '{' classMember* '}'
    ;

classMember
    : modifier* type variableDeclaratorList ';' # FieldDeclaration
    | modifier* methodHeader body # MethodDeclaration
    | classDeclaration # InnerClassDeclaration
    | block # InstanceInitializer
    | 'static' block # StaticInitializer1
    | modifier* constructorDeclarator throwsT? body # ConstructorDeclaration
    | ';' # EmptyDeclaration
    ;

body
    : block
    | ';'
    ;

block
    : '{' blockStatement* '}'
    ;



// Paragraph 14.2
// --------------

blockStatement
    : statement
    ;

statement
    : classDeclaration
    | localVariableDeclarationStatement
    | explicitConstructorInvocation
    | block
    | ';'
    | expressionStatement
    | assertStatement
    | switchStatement
    | doStatement
    | breakStatement
    | continueStatement
    | returnStatement
    | synchronizedStatement
    | throwStatement
    | tryStatement
    | yieldStatement
    | labeledStatement
    | ifThenStatement
    | ifThenElseStatement
    | whileStatement
    | forStatement
    ;



localVariableDeclaration
    : (modifier)* localVariableType variableDeclaratorList?
    ;

localVariableDeclarationStatement
    : localVariableDeclaration ';'
    ;

labeledStatement
    : identifier ':' statement
    ;

// Paragraph 14.8
// --------------

expressionStatement
    : statementExpression ';'
    ;

statementExpression
    : assignment
    | preIncrementExpression
    | preDecrementExpression
    | postIncrementExpression
    | postDecrementExpression
    | methodInvocation
    | classInstanceCreationExpression
    ;

// Paragraph 14.9
// --------------

ifThenStatement
    : 'if' '(' expression ')' statement
    ;

ifThenElseStatement
    : 'if' '(' expression ')' statement 'else' statement
    ;

// Paragraph 14.10
// ---------------

assertStatement
    : 'assert' expression (':' expression)? ';'
    ;

// Paragraph 14.11
// --------------

switchStatement
    : 'switch' '(' expression ')' switchBlock
    ;

switchBlock
    : '{' switchRule switchRule* '}'
    | '{' switchBlockStatementGroup* ( switchLabel ':')* '}'
    ;

switchRule
    : switchLabel '->' (expression ';' | block | throwStatement)
    ;

switchBlockStatementGroup
    : switchLabel ':' (switchLabel ':')* blockStatement+
    ;

switchLabel
    : 'case' caseConstant (',' caseConstant)*
    | 'default'
    ;

caseConstant
    : conditionalExpression
    ;

// Paragraph 14.12
// ---------------

whileStatement
    : 'while' '(' expression ')' statement
    ;

// Paragraph 14.13
// ---------------

doStatement
    : 'do' statement 'while' '(' expression ')' ';'
    ;

// Paragraph 14.14
// ---------------

forStatement
    : basicForStatement
    | enhancedForStatement
    ;


basicForStatement
    : 'for' '(' forInit? ';' expression? ';' forUpdate? ')' statement
    ;

forInit
    : statementExpressionList
    | localVariableDeclaration
    ;

forUpdate
    : statementExpressionList
    ;

statementExpressionList
    : statementExpression (',' statementExpression)*
    ;

enhancedForStatement
    : 'for' '(' localVariableDeclaration ':' expression ')' statement
    ;

// Paragraph 14.15
// ---------------

breakStatement
    : 'break' identifier? ';'
    ;

// Paragraph 14.16
// ---------------

continueStatement
    : 'continue' identifier? ';'
    ;

// Paragraph 14.17
// ---------------

returnStatement
    : 'return' expression? ';'
    ;

// Paragraph 14.18
// ---------------

throwStatement
    : 'throw' expression ';'
    ;

// Paragraph 14.19
// ---------------

synchronizedStatement
    : 'synchronized' '(' expression ')' block
    ;

// Paragraph 14.20
// ---------------

tryStatement
    : 'try' block catches
    | 'try' block finallyBlock
    | 'try' block catches? finallyBlock
    | tryWithResourcesStatement
    ;

catches
    : catchClause catchClause*
    ;

catchClause
    : 'catch' '(' catchFormalParameter ')' block
    ;

catchFormalParameter
    : (modifier)* catchType variableDeclaratorId
    ;

catchType
    : classType ('|' classType)*
    ;

finallyBlock
    : 'finally' block
    ;

tryWithResourcesStatement
    : 'try' resourceSpecification block catches? finallyBlock?
    ;

resourceSpecification
    : '(' resourceList ';'? ')'
    ;

resourceList
    : resource (';' resource)*
    ;

resource
    : localVariableDeclaration
    | identPath
    | fieldAccess
    ;

yieldStatement
    : 'yield' expression ';'
    ;

/////////////////////////////// types

primitiveType
    : 'byte'
    | 'short'
    | 'int'
    | 'long'
    | 'char'
    | 'float'
    | 'double'
    | 'boolean'
    ;

classType
    : genericIdent ('.' genericIdent)*
    ;

genericIdent
    : identifier typeArguments?
    ;

arrayType
    : primitiveType arrayDimension+
    | classType arrayDimension+
    | identifier arrayDimension+
    ;

intersectionType
    : primitiveType ('&' intersectionType)*
    | classType ('&' intersectionType)*
    | identifier ('&' intersectionType)*
    | arrayType ('&' intersectionType)*
    ;

type
    : primitiveType
    | classType
    | identifier
    | arrayType
    | intersectionType
    ;

localVariableType
    : type
    | 'var'
    ;


identifier
    : Identifier
    | 'exports'
    | 'module'
    | 'open'
    | 'opens'
    | 'permits'
    | 'provides'
    | 'record'
    | 'requires'
    | 'sealed'
    | 'to'
    | 'transitive'
    | 'uses'
    | 'var'
    | 'with'
    | 'yield'
    ;

















// Modifiers

modifier
    : 'public'
    | 'protected'
    | 'private'
    | 'static'
    | 'final'
    | 'abstract'
    | 'transient'
    | 'volatile'
    | 'synchronized'
    | 'native'
    | 'sealed'
    | 'non-sealed'
    ;

literal
    : IntegerLiteral
    | FloatingPointLiteral
    | BooleanLiteral
    | CharacterLiteral
    | StringLiteral
    | TextBlock
    | NullLiteral
    ;




// Paragraph 4.3
// -------------


arrayDimension
    : '[' expression? ']'
    ;

// Paragraph 4.4
// -------------

typeParameter
    : identifier typeBound?
    ;

typeBound
    : 'extends' classType intersectionType*
    ;


// Paragraph 4.5.1
// ---------------

typeArguments
    : '<' typeArgumentList '>'
    ;

typeArgumentList
    : typeArgument (',' typeArgument)*
    ;

typeArgument
    : type
    | wildcard
    ;

wildcard
    : '?' wildcardBounds?
    ;

wildcardBounds
    : 'extends' type
    | 'super' type
    ;

// Paragraph 6.5
// -------------

identPath : identifier ('.' identifier)*;

packageDeclaration
    : 'package' identPath ';'
    ;

importDeclaration : 'import' 'static'?  identPath ('.' '*')? ';';

// Paragraph 8.1
// -------------


typeParameters
    : '<' typeParameterList '>'
    ;

typeParameterList
    : typeParameter (',' typeParameter)*
    ;

classExtends
    : 'extends' classType
    ;

classImplements
    : 'implements' interfaceTypeList
    ;

interfaceTypeList
    : classType (',' classType)*
    ;





// Paragraph 8.3
// -------------


variableDeclaratorList
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    : identifier arrayDimension*
    ;

variableInitializer
    : expression
    | arrayInitializer
    ;

// Paragraph 8.4
// -------------


methodHeader
    : (typeParameters)? result methodDeclarator throwsT?
    ;

result
    : type
    | 'void'
    ;

methodDeclarator
    : identifier '(' formalParameterList? ')' arrayDimension*
    ;


formalParameterList
    : formalParameter (',' formalParameter)*
    ;

formalParameter
    : (modifier)* type variableDeclaratorId
    | variableArityParameter
    ;

variableArityParameter
    : (modifier)* type '...' identifier
    ;

throwsT
    : 'throws' exceptionTypeList
    ;

exceptionTypeList
    : exceptionType (',' exceptionType)*
    ;

exceptionType
    : classType
    | identifier
    ;

// Paragraph 8.8
// -------------

constructorDeclarator
    : typeParameters? identifier '(' formalParameterList? ')'
    ;

explicitConstructorInvocation
    : typeArguments? ('this' | 'super') '(' argumentList? ')' ';'
    | (identPath | primary) '.' typeArguments? 'super' '(' argumentList? ')' ';'
    ;

// Paragraph 10.6
// --------------

arrayInitializer
    : '{' variableInitializerList? ','? '}'
    // Strange  ','  ?! staat ook in antlr_java.g4
    ;

variableInitializerList
    : variableInitializer (',' variableInitializer)*
    ;


// Paragraph 14.3
// --------------

// Paragraph 14.4
// --------------

// Paragraph 15.2
// --------------

expression
    : lambdaExpression
    | assignmentExpression
    ;

// Paragraph 15.8
// --------------

primary
    : literal pNNA?
    | classLiteral pNNA?
    | 'this' pNNA?
    | identPath '.' 'this' pNNA?
    | '(' expression ')' pNNA?
    | unqualifiedClassInstanceCreationExpression pNNA?
    | identPath '.' unqualifiedClassInstanceCreationExpression pNNA?
    | arrayCreationExpression '.' unqualifiedClassInstanceCreationExpression pNNA?
    | arrayCreationExpression '.' identifier pNNA?
    | 'super' '.' identifier pNNA?
    | identPath '.' 'super' '.' identifier pNNA?
    | identPath '[' expression ']' pNNA?
    | arrayCreationExpressionWithInitializer '[' expression ']' pNNA?
    | identifier '(' argumentList? ')' pNNA?
    | identPath '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | identPath '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | arrayCreationExpression '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | 'super' '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | identPath '.' 'super' '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | identPath '::' typeArguments? identifier pNNA?
    | arrayCreationExpression '::' typeArguments? identifier pNNA?
    | type '::' typeArguments? identifier pNNA?
    | 'super' '::' typeArguments? identifier pNNA?
    | identPath '.' 'super' '::' typeArguments? identifier pNNA?
    | classType '::' typeArguments? 'new' pNNA?
    | arrayType '::' 'new' pNNA?
    | arrayCreationExpression
    ;

pNNA
    : '.' unqualifiedClassInstanceCreationExpression pNNA?
    | '.' identifier pNNA?
    | '[' expression ']' pNNA?
    | '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | '::' typeArguments? identifier pNNA?
    ;

classLiteral
    : identPath ('[' ']')* '.' 'class'
    | primitiveType ( '[' ']')* '.' 'class'
    | 'void' '.' 'class'
    ;

// Paragraph 15.9
// --------------

classInstanceCreationExpression
    : unqualifiedClassInstanceCreationExpression
    | identPath '.' unqualifiedClassInstanceCreationExpression
    | primary '.' unqualifiedClassInstanceCreationExpression
    ;

unqualifiedClassInstanceCreationExpression
    : 'new' typeArguments? classOrInterfaceTypeToInstantiate '(' argumentList? ')'
    ;

classOrInterfaceTypeToInstantiate
    : identifier ('.' identifier)* typeArgumentsOrDiamond?
    ;

typeArgumentsOrDiamond
    : typeArguments
    | '<>'
    ;

// Paragraph 15.10
// ---------------

arrayCreationExpression
    : arrayCreationExpressionWithoutInitializer
    | arrayCreationExpressionWithInitializer
    ;

arrayCreationExpressionWithoutInitializer
    : 'new' primitiveType arrayDimension+
    | 'new' classType arrayDimension+
    ;

arrayCreationExpressionWithInitializer
    : 'new' primitiveType arrayDimension+ arrayInitializer
    | 'new' classType arrayDimension+ arrayInitializer
    ;

arrayAccess
    : identPath '[' expression ']'
    | primary '[' expression ']'
    | arrayCreationExpressionWithInitializer '[' expression ']'
    ;

// Paragraph 15.11
// ---------------

fieldAccess
    : primary '.' identifier
    | 'super' '.' identifier
    | identPath '.' 'super' '.' identifier
    ;

// Paragraph 15.12
// ---------------

methodInvocation
    : identifier '(' argumentList? ')'
    | identPath '.' typeArguments? identifier '(' argumentList? ')'
    | identPath '.' typeArguments? identifier '(' argumentList? ')'
    | primary '.' typeArguments? identifier '(' argumentList? ')'
    | 'super' '.' typeArguments? identifier '(' argumentList? ')'
    | identPath '.' 'super' '.' typeArguments? identifier '(' argumentList? ')'
    ;

argumentList
    : expression (',' expression)*
    ;

// Paragraph 15.14
// ---------------

postfixExpression
    : primary pfE?
    | identPath pfE?
    ;

pfE
    : '++' pfE?
    | '--' pfE?
    ;

postIncrementExpression
    : postfixExpression '++'
    ;

postDecrementExpression
    : postfixExpression '--'
    ;

// Paragraph 15.15
// ---------------

unaryExpression
    : preIncrementExpression
    | preDecrementExpression
    | '+' unaryExpression
    | '-' unaryExpression
    | castableUnaryExpression
    ;

preIncrementExpression
    : '++' unaryExpression
    ;

preDecrementExpression
    : '--' unaryExpression
    ;

castableUnaryExpression
    : postfixExpression # PosfixUnaryExpession
    | '~' unaryExpression # InvUnaryExpression
    | '!' unaryExpression # NegUnaryExpression
    | castExpression  # CastUnaryExpression
    | 'switch' '(' expression ')' switchBlock  # SwitchUnaryExpression
    ;

// Paragraph 15.16
// ---------------

castExpression
    : '(' type ')' castableUnaryExpression
    ;

// Paragraph 15.17
// ---------------

multiplicativeExpression
    : unaryExpression
    | multiplicativeExpression '*' unaryExpression
    | multiplicativeExpression '/' unaryExpression
    | multiplicativeExpression '%' unaryExpression
    ;

// Paragraph 15.18
// ---------------

additiveExpression
    : multiplicativeExpression
    | additiveExpression '+' multiplicativeExpression
    | additiveExpression '-' multiplicativeExpression
    ;

// Paragraph 15.19
// ---------------

shiftExpression
    : additiveExpression
    | shiftExpression '<' '<' additiveExpression
    | shiftExpression '>' '>' additiveExpression
    | shiftExpression '>' '>' '>' additiveExpression
    ;

// Paragraph 15.20
// ---------------

relationalExpression
    : shiftExpression
    | relationalExpression '<' shiftExpression
    | relationalExpression '>' shiftExpression
    | relationalExpression '<=' shiftExpression
    | relationalExpression '>=' shiftExpression
    //      | instanceofExpression
    | relationalExpression 'instanceof' (type | localVariableDeclaration)
    // Solves left recursion with instanceofExpression.
    ;

// instanceofExpression
//        : relationalExpression 'instanceof' (referenceType | pattern)
//        ;
// Resulted to left recursion with relationalExpression.

// Paragraph 15.21
// ---------------

equalityExpression
    : relationalExpression
    | equalityExpression '==' relationalExpression
    | equalityExpression '!=' relationalExpression
    ;

// Paragraph 15.22
// ---------------

andExpression
    : equalityExpression
    | andExpression '&' equalityExpression
    ;

exclusiveOrExpression
    : andExpression
    | exclusiveOrExpression '^' andExpression
    ;

inclusiveOrExpression
    : exclusiveOrExpression
    | inclusiveOrExpression '|' exclusiveOrExpression
    ;

// Paragraph 15.23
// ---------------

conditionalAndExpression
    : inclusiveOrExpression
    | conditionalAndExpression '&&' inclusiveOrExpression
    ;

// Paragraph 15.24
// ---------------

conditionalOrExpression
    : conditionalAndExpression
    | conditionalOrExpression '||' conditionalAndExpression
    ;

// Paragraph 15.25
// ---------------

conditionalExpression
    : conditionalOrExpression
    | conditionalOrExpression '?' expression ':' conditionalExpression
    | conditionalOrExpression '?' expression ':' lambdaExpression
    ;

// Paragraph 15.26
// ---------------

assignmentExpression
    : conditionalExpression
    | assignment
    ;

assignment
    : leftHandSide assignmentOperator expression
    ;

leftHandSide
    : identPath
    | fieldAccess
    | arrayAccess
    ;

assignmentOperator
    : '='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    ;

// Paragraph 15.27
// ---------------

lambdaExpression
    : lambdaParameters '->' lambdaBody
    ;

lambdaParameters
    : '(' lambdaParameterList? ')'
    | identifier
    ;

lambdaParameterList
    : lambdaParameter (',' lambdaParameter)*
    | identifier ( ',' identifier)*
    ;

lambdaParameter
    : (modifier)* lambdaParameterType variableDeclaratorId
    | variableArityParameter
    ;

lambdaParameterType
    : type
    | 'var'
    ;

lambdaBody
    : expression # ExpressionLambdaBody
    | block # BlockLambdaBody
    ;


// LEXER

EXPORTS    : 'exports';
MODULE     : 'module';
NONSEALED  : 'non-sealed';
OACA       : '<>';
OPEN       : 'open';
OPENS      : 'opens';
PERMITS    : 'permits';
PROVIDES   : 'provides';
RECORD     : 'record';
REQUIRES   : 'requires';
SEALED     : 'sealed';
TO         : 'to';
TRANSITIVE : 'transitive';
USES       : 'uses';
VAR        : 'var';
WITH       : 'with';
YIELD      : 'yield';

// §3.9 Keywords

ABSTRACT     : 'abstract';
ASSERT       : 'assert';
BOOLEAN      : 'boolean';
BREAK        : 'break';
BYTE         : 'byte';
CASE         : 'case';
CATCH        : 'catch';
CHAR         : 'char';
CLASS        : 'class';
CONST        : 'const';
CONTINUE     : 'continue';
DEFAULT      : 'default';
DO           : 'do';
DOUBLE       : 'double';
ELSE         : 'else';
ENUM         : 'enum';
EXTENDS      : 'extends';
FINAL        : 'final';
FINALLY      : 'finally';
FLOAT        : 'float';
FOR          : 'for';
IF           : 'if';
GOTO         : 'goto';
IMPLEMENTS   : 'implements';
IMPORT       : 'import';
INSTANCEOF   : 'instanceof';
INT          : 'int';
INTERFACE    : 'interface';
LONG         : 'long';
NATIVE       : 'native';
NEW          : 'new';
PACKAGE      : 'package';
PRIVATE      : 'private';
PROTECTED    : 'protected';
PUBLIC       : 'public';
RETURN       : 'return';
SHORT        : 'short';
STATIC       : 'static';
STRICTFP     : 'strictfp';
SUPER        : 'super';
SWITCH       : 'switch';
SYNCHRONIZED : 'synchronized';
THIS         : 'this';
THROW        : 'throw';
THROWS       : 'throws';
TRANSIENT    : 'transient';
TRY          : 'try';
VOID         : 'void';
VOLATILE     : 'volatile';
WHILE        : 'while';
UNDER_SCORE  : '_'; //Introduced in Java 9

// §3.10.1 Integer Literals

IntegerLiteral:
    DecimalIntegerLiteral
    | HexIntegerLiteral
    | OctalIntegerLiteral
    | BinaryIntegerLiteral
;

fragment DecimalIntegerLiteral: DecimalNumeral IntegerTypeSuffix?;

fragment HexIntegerLiteral: HexNumeral IntegerTypeSuffix?;

fragment OctalIntegerLiteral: OctalNumeral IntegerTypeSuffix?;

fragment BinaryIntegerLiteral: BinaryNumeral IntegerTypeSuffix?;

fragment IntegerTypeSuffix: [lL];

fragment DecimalNumeral: '0' | NonZeroDigit (Digits? | Underscores Digits);

fragment Digits: Digit (DigitsAndUnderscores? Digit)?;

fragment Digit: '0' | NonZeroDigit;

fragment NonZeroDigit: [1-9];

fragment DigitsAndUnderscores: DigitOrUnderscore+;

fragment DigitOrUnderscore: Digit | '_';

fragment Underscores: '_'+;

fragment HexNumeral: '0' [xX] HexDigits;

fragment HexDigits: HexDigit (HexDigitsAndUnderscores? HexDigit)?;

fragment HexDigit: [0-9a-fA-F];

fragment HexDigitsAndUnderscores: HexDigitOrUnderscore+;

fragment HexDigitOrUnderscore: HexDigit | '_';

fragment OctalNumeral: '0' Underscores? OctalDigits;

fragment OctalDigits: OctalDigit (OctalDigitsAndUnderscores? OctalDigit)?;

fragment OctalDigit: [0-7];

fragment OctalDigitsAndUnderscores: OctalDigitOrUnderscore+;

fragment OctalDigitOrUnderscore: OctalDigit | '_';

fragment BinaryNumeral: '0' [bB] BinaryDigits;

fragment BinaryDigits: BinaryDigit (BinaryDigitsAndUnderscores? BinaryDigit)?;

fragment BinaryDigit: [01];

fragment BinaryDigitsAndUnderscores: BinaryDigitOrUnderscore+;

fragment BinaryDigitOrUnderscore: BinaryDigit | '_';

// §3.10.2 Floating-Point Literals

FloatingPointLiteral: DecimalFloatingPointLiteral | HexadecimalFloatingPointLiteral;

fragment DecimalFloatingPointLiteral
    : Digits '.' Digits? ExponentPart? ([fFdD])?
    | '.' Digits ExponentPart? ([fFdD])?
    | Digits ExponentPart ([fFdD])?
    | Digits [fFdD]
    ;

fragment ExponentPart: [eE] SignedInteger;

fragment SignedInteger: [+-]? Digits;

fragment HexadecimalFloatingPointLiteral: HexSignificand [pP] SignedInteger [fFdD]?;

fragment HexSignificand: HexNumeral '.'? | '0' [xX] HexDigits? '.' HexDigits;

BooleanLiteral: 'true' | 'false';

CharacterLiteral: '\'' SingleCharacter '\'' | '\'' EscapeSequence '\'';

fragment SingleCharacter: ~['\\\r\n];

// §3.10.5 String Literals

StringLiteral: '"' StringCharacters? '"';

fragment StringCharacters: StringCharacter+;

fragment StringCharacter: ~["\\\r\n] | EscapeSequence;

TextBlock: '"""' [ \t]* [\n\r] [.\r\b]* '"""';

// §3.10.6 Escape Sequences for Character and String Literals

fragment EscapeSequence:
    '\\' [btnfr"'\\]
    | OctalEscape
    | UnicodeEscape // This is not in the spec but prevents having to preprocess the input
;

fragment OctalEscape:
    '\\' OctalDigit
    | '\\' OctalDigit OctalDigit
    | '\\' ZeroToThree OctalDigit OctalDigit
;

fragment ZeroToThree: [0-3];

// This is not in the spec but prevents having to preprocess the input
fragment UnicodeEscape: '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit;

// §3.10.7 The Null Literal

NullLiteral: 'null';

// §3.11 Separators

LPAREN     : '(';
RPAREN     : ')';
LBRACE     : '{';
RBRACE     : '}';
LBRACK     : '[';
RBRACK     : ']';
SEMI       : ';';
COMMA      : ',';
DOT        : '.';
ELLIPSIS   : '...';
AT         : '@';
COLONCOLON : '::';

// §3.12 Operators

ASSIGN   : '=';
GT       : '>';
LT       : '<';
BANG     : '!';
TILDE    : '~';
QUESTION : '?';
COLON    : ':';
ARROW    : '->';
EQUAL    : '==';
LE       : '<=';
GE       : '>=';
NOTEQUAL : '!=';
AND      : '&&';
OR       : '||';
INC      : '++';
DEC      : '--';
ADD      : '+';
SUB      : '-';
MUL      : '*';
DIV      : '/';
BITAND   : '&';
BITOR    : '|';
CARET    : '^';
MOD      : '%';

ADD_ASSIGN     : '+=';
SUB_ASSIGN     : '-=';
MUL_ASSIGN     : '*=';
DIV_ASSIGN     : '/=';
AND_ASSIGN     : '&=';
OR_ASSIGN      : '|=';
XOR_ASSIGN     : '^=';
MOD_ASSIGN     : '%=';
LSHIFT_ASSIGN  : '<<=';
RSHIFT_ASSIGN  : '>>=';
URSHIFT_ASSIGN : '>>>=';

Identifier: [$A-Za-z_][$A-Za-z_0-9]*;

WS: [ \t\r\n\u000C]+ -> skip;

COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

LINE_COMMENT: '//' ~[\r\n]* -> channel(HIDDEN);