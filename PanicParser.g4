/*
 Copyright (c) 2010 The Rust Project Developers Copyright (c) 2020-2022 Student Main
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice (including the next paragraph) shall be
 included in all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar PanicParser;

options
{
    tokenVocab = PanicLexer;
}

// entry point
// 4
crate
    : item* EOF
    ;

// 6
item
    : function_
    ;

// 6.4
function_
    : 'fn' identifier '(' functionParameters? ')' functionReturnType block
    ;

functionParameters
    : functionParam (',' functionParam)* ','?
    ;

functionParam
    : identifier ':' type_
    ;

functionReturnType
    : '->' type_
    ;

// 8
statement
    : ';'
    | letStatement
    | returnStatement
    | continueStatement
    | breakStatement
    | assignmentStatement
    | loopBlock
    | ifBlock
    | block
    ;

letStatement
    : 'let' 'mut'? identifier ':' type_ '=' expression ';'
    ;

returnStatement
    : 'return' expression ';'
    ;

continueStatement
    : 'continue' LIFETIME_OR_LABEL? ';'
    ;

breakStatement
    : 'break' LIFETIME_OR_LABEL? ';'
    ;

assignmentStatement
    : identifier '=' expression ';'
    ;

// 8.2
expression
    : literalExpression                        # LiteralExouterAttributepression_
    | identifier '(' callParams? ')'           # CallExpression                // 8.2.9
    | ('-' | '!') expression                   # NegationExpression            // 8.2.4
    | expression ('*' | '/' | '%') expression  # ArithmeticOrLogicalExpression // 8.2.4
    | expression ('+' | '-') expression        # ArithmeticOrLogicalExpression // 8.2.4
    | expression (shl | shr) expression        # ArithmeticOrLogicalExpression // 8.2.4
    | expression '&' expression                # ArithmeticOrLogicalExpression // 8.2.4
    | expression '^' expression                # ArithmeticOrLogicalExpression // 8.2.4
    | expression '|' expression                # ArithmeticOrLogicalExpression // 8.2.4
    | expression comparisonOperator expression # ComparisonExpression          // 8.2.4
    | expression '&&' expression               # LazyBooleanExpression         // 8.2.4
    | expression '||' expression               # LazyBooleanExpression         // 8.2.4
    | '(' expression ')'                       # GroupedExpression             // 8.2.5    
    ;

comparisonOperator
    : '=='
    | '!='
    | '>'
    | '<'
    | '>='
    | '<='
    ;

// 8.2.1
literalExpression
    : CHAR_LITERAL
    | STRING_LITERAL
    | RAW_STRING_LITERAL
    | BYTE_LITERAL
    | BYTE_STRING_LITERAL
    | RAW_BYTE_STRING_LITERAL
    | INTEGER_LITERAL
    | FLOAT_LITERAL
    | KW_TRUE
    | KW_FALSE
    ;

// 8.2.3
block
    : '{' statements? '}'
    ;

statements
    : statement+
    ;

// 8.2.9
callParams
    : expression (',' expression)* ','?
    ;

// 8.2.13
loopBlock
    : loopLabel? (infiniteLoopBlock | predicateLoopBlock)
    ;

infiniteLoopBlock
    : 'loop' block
    ;

predicateLoopBlock
    : 'while' expression block
    ;

loopLabel
    : LIFETIME_OR_LABEL ':'
    ;

// 8.2.15
ifBlock
    : 'if' expression block ('else' (block | ifBlock))?
    ;

// 10.1
type_
    : 'nom'? identifier
    ;

// technical
identifier
    : NON_KEYWORD_IDENTIFIER
    | RAW_IDENTIFIER
    ;

keyword
    : KW_AS
    | KW_BREAK
    | KW_CONTINUE
    | KW_ELSE
    | KW_FALSE
    | KW_FN
    | KW_IF
    | KW_LET
    | KW_LOOP
    | KW_MUT
    | KW_NOM
    | KW_RETURN
    | KW_TRUE
    | KW_WHILE
    ;

// LA can be removed, legal rust code still pass but the cost is `let c = a < < b` will pass... i hope antlr5 can add
// some new syntax? dsl? for these stuff so i needn't write it in (at least) 5 language

shl
    : '<' {this.next('<')}? '<'
    ;

shr
    : '>' {this.next('>')}? '>'
    ;