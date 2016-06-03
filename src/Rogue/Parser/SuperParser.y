{
module Rogue.Parser.SuperParser
    ( parseRogue
    ) where

import           Rogue.Parser.Lexer       (lexer)
import           Rogue.Parser.ParserMonad (ParserM, reportError)
import           Rogue.Parser.Tokens      (BracesToken (..), CmpToken (..),
                                           ControlFlowToken (..),
                                           LogicalToken (..), MathToken (..),
                                           MutabilityToken (..), SeparateToken (..),
                                           StatementToken (..), Token (..),
                                           TypeToken (..), ValueToken (..))
import           Rogue.AST.Untyped        (Declaration (..), Expr (..), FunctionType,
                                           Program, Statement (..), Statements)
}

%name parseRogue
%tokentype { Token }

%monad { ParserM }
%lexer { lexer } { Sep TokenEOF }
%error { parseError }

%token
    let             { Mut TokenLet }
    mut             { Mut TokenMut }

    int             { Val (TokenInt $$) }
    bool            { Val (TokenBool $$) }

    var             { Var $$ }

    'Bool'          { Ty TokenBoolType }
    'Int'           { Ty TokenIntType }
    'Unit'          { Ty TokenUnitType }

    ','             { Sep TokenComma }
    ':'             { Sep TokenColon }
    ';'             { Sep TokenSemicolon }
    '->'            { Sep TokenTypeArrow }
    eol             { Sep TokenEOL }

    '('             { Braces TokenRoundOP }
    ')'             { Braces TokenRoundCL }
    '{'             { Braces TokenCurlyOP }
    '}'             { Braces TokenCurlyCL }

    '&&'            { Logic TokenAnd }
    '||'            { Logic TokenOr }
    '!'             { Logic TokenNot }

    '=='            { Cmp TokenEQ }
    '!='            { Cmp TokenNEQ }
    '<='            { Cmp TokenLEQ }
    '>='            { Cmp TokenGEQ }
    '<'             { Cmp TokenLT }
    '>'             { Cmp TokenGT }

    '+'             { Math TokenPlus }
    '-'             { Math TokenMinus }
    '*'             { Math TokenTimes }
    '/'             { Math TokenDiv }
    '%'             { Math TokenMod }
    '**'            { Math TokenPow }

    '='             { Stmt TokenAssignment }

    if              { Flow TokenIf }
    else            { Flow TokenElse }
    while           { Flow TokenWhile }
    return          { Flow TokenReturn }


%right '&&' '||'
%nonassoc '==' '!=' '<=' '>=' '>' '<'
%left '+' '-'
%left '*' '/' '%' '**'
%left '!'
%left UMINUS
--%left CALL

%%

ProgramFile :: { Program }
ProgramFile : GlobalStatements { concat $ reverse $1 }

GlobalStatements :: { [Program] }
GlobalStatements : {- empty -}                    {      [] }
                 | GlobalStatements eol           {      $1 }
                 | GlobalStatements GlobalStmt    { $2 : $1 }

GlobalStmt :: { Program }
GlobalStmt : VarStmt    { $1 }
           | FunStmt    { $1 }

StmtEnd :: { () }
StmtEnd : eol   { () }
        | ';'   { () }

{- Variabels definition -}
VarStmt :: { Program }
VarStmt : let VarList StmtEnd    { reverse $2 }  -- TODO: deal with mutability here
        | mut VarList StmtEnd    { reverse $2 }

VarList :: { Program }
VarList : VarSingle                {      [$1] }
        | VarList ',' VarSingle    { $3 :  $1  }

VarSingle :: { Declaration }
VarSingle : var OptionalType '=' Expr   { VarDef { mutability = TokenLet, defName = $1, declaredType = $2, varValue = $4 } }

{- Expression section -}
Expr :: { Expr }  -- TODO: linebreaks
Expr : Expr '&&' Expr           { And $1 $3 }
     | Expr '||' Expr           { Or  $1 $3 }
     | '!' Expr                 { Not $2 }

     | Expr '==' Expr           { Equal     $1 $3 }
     | Expr '!=' Expr           { NotEqual  $1 $3 }
     | Expr '<=' Expr           { LowerEq   $1 $3 }
     | Expr '<'  Expr           { Lower     $1 $3 }
     | Expr '>=' Expr           { GreaterEq $1 $3 }
     | Expr '>'  Expr           { Greater   $1 $3 }

     | Expr '+' Expr            { Plus     $1 $3 }
     | Expr '-' Expr            { Minus    $1 $3 }
     | Expr '*' Expr            { Times    $1 $3 }
     | Expr '/' Expr            { Division $1 $3 }
     | Expr '%' Expr            { Modulo   $1 $3 }
     | Expr '**' Expr           { Power    $1 $3 }
     | '-' Expr %prec UMINUS    { Negate   $2    }

     | FunCall                  { $1 }
     | ExprAtom                 { $1 }

ExprAtom :: { Expr }
ExprAtom : int             { IntConst  $1 }
         | bool            { BoolConst $1 }
         | '(' Expr ')'    {           $2 }

FunCall :: { Expr }
FunCall : var FunArgsList    { VarOrCall $1 (reverse $2) }

FunArgsList :: { [Expr] }
FunArgsList : {- empty -}             {                   [] }
            | FunArgsList ExprAtom    {              $2 : $1 }
            | FunArgsList var         { VarOrCall $2 [] : $1 }

{- Type section -}
OptionalType :: { Maybe TypeToken }
OptionalType : ':' BasicType    { Just $2 }  -- TODO: only simple types allowed for variable now
             -- | {- empty -}      { Nothing }

BasicType :: { TypeToken }
BasicType : 'Bool'    { TokenBoolType }
          | 'Int'     { TokenIntType }
          | 'Unit'    { TokenUnitType }

FunTypeArguments :: { FunctionType }
FunTypeArguments : {- empty -}                                        {            [] }
                 | FunTypeArguments '(' var ':' BasicType ')' '->'    { ($3, $5) : $1 }

{- Function section -}
FunStmt :: { Program }
FunStmt : var ':' FunTypeArguments BasicType BlockBody    { [FunDef {defName = $1,  arguments = reverse $3, returnType = $4, funBody = $5 }] }

BlockBody :: { Statements }
BlockBody : '{' BodyStmt '}'    { concat $ reverse $2 }

BodyStmt :: { [Statements] }
BodyStmt : {- empty -}                                  {                 [] }
         | BodyStmt eol                                 {                 $1 }
         | BodyStmt VarStmt                             { (map Def $2)  : $1 }
         | BodyStmt FunCall                             { [Exp $2]      : $1 }
         | BodyStmt var '=' Expr                        { [Ass $2 $4]   : $1 }
         | BodyStmt if Expr BlockBody else BlockBody    { [If $3 $4 $6] : $1 }
         | BodyStmt while Expr BlockBody                { [While $3 $4] : $1 }
         | BodyStmt return RetExpr                      { [Ret $3]      : $1 }

RetExpr :: { Maybe Expr }
RetExpr : {- empty -}    { Nothing }
        | Expr           { Just $1 }

{
parseError :: Token -> ParserM a
parseError token = reportError $ "Error on token: " ++ show token
}