grammar SafeC;

translationUnit: externalDeclaration*;

externalDeclaration
    : functionDefinition
    | declaration
    | Semi;

functionDefinition: declarationSpecifier+ declarator declaration* compoundStatement;

declaration: declarationSpecifier+ initDeclarator (Comma initDeclarator)* Semi;

declarationSpecifier
    : storageClassSpecifier
    | typeSpecifier
    | typeQualifier
    | functionSpecifier;

storageClassSpecifier
    : Extern
    | Static;

typeSpecifier
    : Void
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double
    | Signed
    | Unsigned
    | structOrUnionSpecifier
    | enumSpecifier;

structOrUnionSpecifier
    : (Struct | Union) Identifier? LeftBrace declaration+ RightBrace
    | (Struct | Union) Identifier;

enumSpecifier
    : Enum Identifier? LeftBrace enumerator (Comma enumerator)* Comma? RightBrace
    | Enum Identifier;

enumerator: Identifier (Equal constantExpression)?;

typeQualifier: Const | Volatile | NotNull;

functionSpecifier: Inline;

initDeclarator: declarator (Equal initializer)?;

declarator: (Ast typeQualifier*)* directDeclarator;

directDeclarator
    : Identifier
    | directDeclarator NotNull? LeftBracket RightBracket
    | directDeclarator LeftBracket Ellipsis RightBracket
    | directDeclarator LeftBracket assignmentExpression RightBracket
    | directDeclarator LeftParen parameterList? RightParen
    | LeftParen declarator RightParen;

abstractDeclarator: (Ast typeQualifier*)* directAbstractDeclarator;

directAbstractDeclarator
    : NotNull? LeftBracket RightBracket
    | LeftBracket Ellipsis RightBracket
    | LeftBracket assignmentExpression RightBracket
    | LeftParen parameterList? RightParen
    | directAbstractDeclarator NotNull? LeftBracket assignmentExpression? RightBracket
    | directAbstractDeclarator LeftParen parameterList? RightParen
    | LeftParen abstractDeclarator RightParen;

parameterList: parameterDeclaration (Comma parameterDeclaration)* (Comma Ellipsis)?;

parameterDeclaration: declarationSpecifier+ (declarator | abstractDeclarator?);

initializer
    : assignmentExpression
    | LeftBrace initializer (Comma initializer)* Comma? RightBrace;

statement
    : labeledStatement
    | compoundStatement
    | expressionStatement
    | selectionStatement
    | iterationStatement
    | jumpStatement;

labeledStatement
    : Identifier Colon statement?
    | Case constantExpression Colon statement
    | Default Colon statement;

compoundStatement: LeftBrace blockItem* RightBrace;

blockItem: statement | declaration;

expressionStatement: expression? Semi;

selectionStatement
    : If LeftParen expression RightParen statement (Else statement)?
    | Switch LeftParen expression RightParen statement;

iterationStatement
    : While LeftParen expression RightParen statement
    | Do statement While LeftParen expression RightParen Semi
    | For LeftParen forCondition RightParen statement;

forCondition: (forDeclaration | expression?) Semi expression? Semi expression?;

forDeclaration: declarationSpecifier+ initDeclarator (Comma initDeclarator)*;

jumpStatement
    : Goto Identifier Semi
    | Continue Semi
    | Break Semi
    | Return expression? Semi;

expression: assignmentExpression (Comma assignmentExpression)*;

assignmentExpression
    : conditionalExpression
    | unaryExpression assignmentOperator assignmentExpression;

assignmentOperator
    : Equal
    | PlusEqual
    | HyphEqual
    | AstEqual
    | SlashEqual
    | PercentEqual
    | AmpEqual
    | CaretEqual
    | BarEqual
    | LessLessEqual
    | GreaterGreaterEqual;

constantExpression: conditionalExpression;

conditionalExpression: logicalOrExpression (Question expression Colon conditionalExpression)?;

logicalOrExpression: logicalAndExpression (BarBar logicalAndExpression)*;

logicalAndExpression: inclusiveOrExpression (AmpAmp inclusiveOrExpression)*;

inclusiveOrExpression: exclusiveOrExpression (Bar exclusiveOrExpression)*;

exclusiveOrExpression: andExpression (Caret andExpression)*;

andExpression: equalityExpression (Amp equalityExpression)*;

equalityExpression: relationalExpression ((EqualEqual | BangEqual) relationalExpression)*;

relationalExpression: shiftExpression ((Less | Greater | LessLess | GreaterGreater) shiftExpression)*;

shiftExpression: additiveExpression ((LessLess | GreaterGreater) additiveExpression)*;

additiveExpression: multiplicativeExpression ((Plus | Hyph) multiplicativeExpression)*;

multiplicativeExpression: castExpression ((Ast | Slash | Percent) castExpression)*;

castExpression
    : LeftParen typeName RightParen castExpression
    | unaryExpression;

unaryExpression: (PlusPlus | HyphHyph | Sizeof | Alignof | Countof)* (
    postfixExpression
    | unaryOperator castExpression
    | (Sizeof | Alignof) LeftParen typeName RightParen
);

unaryOperator: Plus | Hyph | Tilde | Bang | Ast | Amp;

postfixExpression: primaryExpression (
    LeftBracket expression RightBracket
    | LeftBracket expression? Colon expression? RightBracket
    | (Dot | Arrow) Identifier
    | PlusPlus
    | HyphHyph
)*;

primaryExpression
    : Identifier
    | IntegerConstant
    | FloatingConstant
    | CharacterConstant
    | StringLiteral+
    | Null
    | LeftParen expression RightParen;

typeName: (typeSpecifier | typeQualifier)+ abstractDeclarator?;

Alignof: 'alignof';
Break: 'break';
Case: 'case';
Char: 'char';
Const: 'const';
Continue: 'continue';
Countof: 'countof';
Default: 'default';
Do: 'do';
Double: 'double';
Else: 'else';
Enum: 'enum';
Extern: 'extern';
Float: 'float';
For: 'for';
Goto: 'goto';
If: 'if';
Inline: 'inline';
Int: 'int';
Long: 'long';
NotNull: 'not_null';
Null: 'null';
Restrict: 'restrict';
Return: 'return';
Short: 'short';
Signed: 'signed';
Sizeof: 'sizeof';
Static: 'static';
Struct: 'struct';
Switch: 'switch';
Union: 'union';
Unsigned: 'unsigned';
Void: 'void';
Volatile: 'volatile';
While: 'while';

LeftParen: '(';
RightParen: ')';
LeftBracket: '[';
RightBracket: ']';
LeftBrace: '{';
RightBrace: '}';
Tilde: '~';
Bang: '!';
Plus: '+';
Hyph: '-';
Ast: '*';
Slash: '/';
Percent: '%';
Amp: '&';
Caret: '^';
Bar: '|';
LessLess: '<<';
GreaterGreater: '>>';
EqualEqual: '==';
BangEqual: '!=';
Less: '<';
LessEqual: '<=';
Greater: '>';
GreaterEqual: '>=';
AmpAmp: '&&';
BarBar: '||';
Equal: '=';
PlusEqual: '+=';
HyphEqual: '-=';
AstEqual: '*=';
SlashEqual: '/=';
PercentEqual: '%=';
AmpEqual: '&=';
CaretEqual: '^=';
BarEqual: '|=';
LessLessEqual: '<<=';
GreaterGreaterEqual: '>>=';
PlusPlus: '++';
HyphHyph: '--';
Question: '?';
Colon: ':';
Comma: ',';
Dot: '.';
Arrow: '->';
Ellipsis: '...';
Semi: ';';

Identifier: [a-zA-Z_] [a-zA-Z_0-9]*;

IntegerConstant: (DecimalConstant | OctalConstant | HexadecimalConstant) IntegerSuffix?;

fragment DecimalConstant: [1-9] [0-9]*;

fragment OctalConstant: '0' [0-9]*;

fragment HexadecimalConstant: '0' [xX] [0-9a-fA-F]+;

fragment IntegerSuffix
    : UnsignedSuffix (LongSuffix | LongLongSuffix)?
    | (LongSuffix | LongLongSuffix) UnsignedSuffix?;

fragment UnsignedSuffix: [uU];

fragment LongSuffix: [lL];

fragment LongLongSuffix: 'll' | 'LL';

FloatingConstant: DecimalFloatingConstant | HexadecimalFloatingConstant;

fragment DecimalFloatingConstant: FractionalConstant ExponentPart? FloatingSuffix?;

fragment HexadecimalFloatingConstant: '0' [xX] HexadecimalFractionalConstant BinaryExponentPart FloatingSuffix?;

fragment FractionalConstant: [0-9]+ | [0-9]+ '.' | '.' [0-9]+ | [0-9+] '.' [0-9]+;

fragment HexadecimalFractionalConstant: [0-9a-fA-F]+ | [0-9a-fA-F]+ '.' | '.' [0-9a-fA-F]+ | [0-9a-fA-F] '.' [0-9a-fA-F];

fragment ExponentPart: [eE] [+-]? [0-9]+;

fragment BinaryExponentPart: [pP] [+-]? [0-9]+;

fragment FloatingSuffix: [fFlL];

CharacterConstant: '\'' CChar+ '\'' | 'L\'' CChar+ '\'';

fragment CChar: ~['\\\r\n] | EscapeSequence;

StringLiteral: '"' SChar* '"' | 'L"' SChar* '"';

fragment SChar: ~["\\\r\n] | EscapeSequence;

fragment EscapeSequence
    : SimpleEscapeSequence
    | OctalEscapeSequence
    | HexadecimalEscapeSequence
    | UniversalCharacterName;

fragment SimpleEscapeSequence: '\\' ['"?abfnrtv\\];

fragment OctalEscapeSequence: '\\' [0-7] [0-7]? [0-7]?;

fragment HexadecimalEscapeSequence: '\\x' [0-9a-fA-F]+;

fragment UniversalCharacterName
    : '\\u' [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]
    | '\\U' [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F];

Whitespace: [ \t\r\n]+ -> skip;
