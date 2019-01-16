%skeleton "lalr1.cc"

%define parser_class_name{Parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define parse.error verbose

%locations

%code requires {
#include<map>
#include<set>
#include<list>
#include<memory>
#include<string>
#include<vector>
#include<variant>
#include<iostream>
#include<algorithm>

struct Identifier: std::string {
  Identifier() = default;
  Identifier(std::string const &s): std::string{s} { }
  operator std::string_view const() { return std::string::operator std::string_view(); }
};

struct Qualifier {
  enum struct Type: std::uint8_t {
    None     = 0,
    Const    = 1 << 0,
    Volatile = 1 << 1,
    Mutable  = 1 << 2,
    Override = 1 << 3,
  };
  Type type;
};

struct StorageSpecifier {
  enum struct Type: std::uint8_t {
    None        = 0,
    Static      = 1 << 0,
    ThreadLocal = 1 << 1,
    Register    = 1 << 2,
    Extern      = 1 << 3,
  };
  Type type;
};

struct Expression;

struct Type {
  std::shared_ptr<Expression> type;
  Qualifier qual;
  StorageSpecifier storage;
};

struct Declaration {
  std::shared_ptr<Expression> type;
  Identifier ident;
  std::vector<Expression> arguments;
};

struct Addition       { std::shared_ptr<Expression> lhs, rhs; };
struct Subtraction    { std::shared_ptr<Expression> lhs, rhs; };
struct Multiplication { std::shared_ptr<Expression> lhs, rhs; };
struct Division       { std::shared_ptr<Expression> lhs, rhs; };
struct Modulus        { std::shared_ptr<Expression> lhs, rhs; };

struct Equals    { std::shared_ptr<Expression> lhs, rhs; };
struct NotEquals { std::shared_ptr<Expression> lhs, rhs; };

struct LogicalAnd { std::shared_ptr<Expression> lhs, rhs; };
struct LogicalOr  { std::shared_ptr<Expression> lhs, rhs; };

struct BitwiseAnd { std::shared_ptr<Expression> lhs, rhs; };
struct BitwiseOr  { std::shared_ptr<Expression> lhs, rhs; };
struct BitwiseXor { std::shared_ptr<Expression> lhs, rhs; };

struct UnaryMinus  { std::shared_ptr<Expression> expr; };
struct AddressOf   { std::shared_ptr<Expression> expr; };
struct Dereference { std::shared_ptr<Expression> expr; };
struct UnaryNot    { std::shared_ptr<Expression> expr; };

struct Assignment { std::shared_ptr<Expression> lhs, rhs; };

struct TemplateInst { std::shared_ptr<Expression> templ; std::vector<Expression> arguments; };
struct FunctionCall { std::shared_ptr<Expression> functor; std::vector<Expression> arguments; };

struct Return       { std::shared_ptr<Expression> expr; };
struct MemberAccess { std::shared_ptr<Expression> lhs; std::string rhs; };

struct Preincrement  { std::shared_ptr<Expression> expr; };
struct Predecrement  { std::shared_ptr<Expression> expr; };
struct Postincrement { std::shared_ptr<Expression> expr; };
struct Postdecrement { std::shared_ptr<Expression> expr; };

//struct CommaExpression { std::shared_ptr<Expression> lhs, rhs; };

struct NoOp { };

using ExprT = std::variant<Identifier
                         , NoOp
                         , Addition
                         , Subtraction
                         , Multiplication
                         , Division
                         , Modulus
                         , Equals
                         , NotEquals
                         , LogicalAnd
                         , LogicalOr
                         , BitwiseAnd
                         , BitwiseOr
                         , BitwiseXor
                         , UnaryMinus
                         , AddressOf
                         , Dereference
                         , UnaryNot
                         , Assignment
                         , TemplateInst
                         , FunctionCall
                         , Return
                         , MemberAccess
                         , Preincrement
                         , Predecrement
                         , Postincrement
                         , Postdecrement
                       //, CommaExpression
                         , Type
                         , std::vector<Declaration>
                         , std::string
                         , int
                         , unsigned
                         , long long
                         , unsigned long long
                         , float
                         , double
>;

struct Expression: ExprT { using ExprT::ExprT; };

struct Statement;

struct IfElse {
  Expression condition;
  std::shared_ptr<Statement> taken;
  std::shared_ptr<Statement> notTaken;
};

struct For {
  Expression initialization;
  Expression condition;
  Expression loopEnd;
  std::shared_ptr<Statement> loopBody;
};

struct DoWhile {
  std::shared_ptr<Statement> loopBody;
  Expression condition;
};

struct CompoundStatement {
  std::vector<Statement> statements;
};

struct Strong {
  std::string typeType;
  std::string identifier;
  std::shared_ptr<Statement> contents;
};

using StatementT = std::variant<CompoundStatement
                              , Strong
                              , Expression
                              , For
                              , DoWhile
                              , IfElse
>;

struct Statement: StatementT { using StatementT::StatementT; };

std::ostream &operator<<(std::ostream &os, Qualifier const &q) {
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Const)) os << " const";
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Volatile)) os << " volatile";
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Mutable)) os << " mutable";
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Override)) os << " override";
  return os;
}

std::ostream &operator<<(std::ostream &os, StorageSpecifier const &q) {
  if(static_cast<int>(q.type) & static_cast<int>(StorageSpecifier::Type::Static)) os << "static ";
  if(static_cast<int>(q.type) & static_cast<int>(StorageSpecifier::Type::ThreadLocal)) os << "thread_local ";
  if(static_cast<int>(q.type) & static_cast<int>(StorageSpecifier::Type::Register)) os << "register ";
  if(static_cast<int>(q.type) & static_cast<int>(StorageSpecifier::Type::Extern)) os << "extern ";
  return os;
}

#include <iterator>

template<typename Container>
void printExpressions(std::ostream &os, Container const &cnt, char const *delim);

std::ostream &operator<<(std::ostream &os, ExprT const &ex) {
  //os << '(';
  std::visit([&os](auto ex) {
    if constexpr(std::is_same_v<decltype(ex), Identifier>) { os << '[' << ex << ']'; }
    else if constexpr(std::is_same_v<decltype(ex), NoOp>) { }
    else if constexpr(std::is_same_v<decltype(ex), Addition>)       { os << *ex.lhs << '+' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Subtraction>)    { os << *ex.lhs << '-' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Multiplication>) { os << *ex.lhs << '*' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Division>)       { os << *ex.lhs << '/' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Modulus>)        { os << *ex.lhs << '%' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Equals>)     { os << *ex.lhs << "==" << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), NotEquals>)  { os << *ex.lhs << "!=" << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), LogicalAnd>) { os << *ex.lhs << "&&" << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), LogicalOr>)  { os << *ex.lhs << "||" << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), BitwiseAnd>) { os << *ex.lhs << '&' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), BitwiseOr>)  { os << *ex.lhs << '|' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), BitwiseXor>) { os << *ex.lhs << '^' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), UnaryMinus>)  { os << '-' << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), AddressOf>)   { os << '&' << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), Dereference>) { os << '*' << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), UnaryNot>)    { os << '!' << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), Assignment>)   { os << *ex.lhs << '=' << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), FunctionCall>) { os << *ex.functor << '('; printExpressions(os, ex.arguments, ","); os << ')'; }
    else if constexpr(std::is_same_v<decltype(ex), TemplateInst>) { os << *ex.templ   << '<'; printExpressions(os, ex.arguments, ","); os << '>'; }
    else if constexpr(std::is_same_v<decltype(ex), Return>) { os << "return " << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), MemberAccess>) { os << *ex.lhs << '.' << ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Preincrement>)  { os << "++" << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), Predecrement>)  { os << "--" << *ex.expr; }
    else if constexpr(std::is_same_v<decltype(ex), Postincrement>) { os << *ex.expr << "++"; }
    else if constexpr(std::is_same_v<decltype(ex), Postdecrement>) { os << *ex.expr << "--"; }
  //else if constexpr(std::is_same_v<decltype(ex), CommaExpression>) { os << *ex.lhs << ", " << *ex.rhs; }
    else if constexpr(std::is_same_v<decltype(ex), Type>) { os << *ex.type << ex.qual; }
    else if constexpr(std::is_same_v<decltype(ex), Declaration>) {
      if(ex.valueAssigned) os << "decl: " << *ex.type << " " << ex.ident << '{' << *ex.valueAssigned << '}';
      else                 os << "decl: " << *ex.type << " " << ex.ident;
    }
    else if constexpr(std::is_same_v<decltype(ex), std::string>) { os << '"' << ex << '"'; }
    else if constexpr(std::is_same_v<decltype(ex), int>) { os << ex << 'i'; }
    else if constexpr(std::is_same_v<decltype(ex), unsigned>) { os << ex << 'u'; }
    else if constexpr(std::is_same_v<decltype(ex), long long>) { os << ex << "ll"; }
    else if constexpr(std::is_same_v<decltype(ex), unsigned long long>) { os << ex << "ull"; }
    else if constexpr(std::is_same_v<decltype(ex), float>) { os << ex << 'f'; }
    else if constexpr(std::is_same_v<decltype(ex), double>) { os << ex << 'd'; }
    else { os << "Unknown expression"; }
  }, ex);
  //os << ')';
  return os;
}

template<typename Container>
void printExpressions(std::ostream &os, Container const &cnt, char const *delim) {
  if(!std::distance(cnt.begin(), cnt.end())) return;
  std::copy(cnt.begin(), cnt.end()-1, std::ostream_iterator<ExprT>(os, delim));
  os << cnt.back();
}

std::ostream &operator<<(std::ostream &os, StatementT const &stmt) {
  std::visit([&os](auto st){
    if constexpr(std::is_same_v<decltype(st), CompoundStatement>) { os << '{'; for(auto &s: st.statements) os << s; os << '}'; }
    else if constexpr(std::is_same_v<decltype(st), Strong>) { os << "strong " << st.typeType << " type " << st.identifier << ": " << *st.contents; }
    else if constexpr(std::is_same_v<decltype(st), For>) { os << "for(" << st.initialization << ", " << st.condition << ", " << st.loopEnd << ") " << *st.loopBody; }
    else if constexpr(std::is_same_v<decltype(st), DoWhile>) { os << "do " << st.loopBody << " while (" << st.condition << ')'; }
    else if constexpr(std::is_same_v<decltype(st), IfElse>) { os << "if(" << st.condition << ") " << *st.taken << " else " << *st.notTaken; }
    else if constexpr(std::is_same_v<decltype(st), Expression>) { os << st << ';'; }
    else { os << "Unknown statement"; }
  }, stmt);
  return os;
}

struct LexerContext;
}//%code requires

%param { LexerContext &context }

%code {
struct LexerContext {
  std::vector<Statement> topLevelStatements;
  char const *cursor;
  yy::location loc;
};

namespace yy { Parser::symbol_type yylex(LexerContext &ctx); }

#define M(x) std::move(x)
}//%code

%token END 0 "end of file"
%token IDENTIFIER
%token RETURN "return" IF "if" ELSE "else" DO "do" WHILE "while" FOR "for" POINTER "ptr" REFERENCE "ref" VALUE "val" TYPE "type" FUNC "func"
%token OR "||" AND "&&" EQ "==" NE "!=" INC "++" DEC "--" PLEQ "+=" MIEQ "-=" THREEWAY "<=>" STRONG "strong"
%token CONST "const" VOLATILE "volatile" STATIC "static" MUTABLE "mutable" OVERRIDE "override" THREADLOCAL "thread_local" REGISTER "register" EXTERN "extern"
%token INTLITERAL CHARLITERAL UNSIGNEDLITERAL LONGLONGLITERAL UNISGNEDLONGLONGLITERAL FLOATLITERAL DOUBLELITERAL

%left ','
%right '?' ':' '=' "+=" "-="
%left "||"
%left "&&"
%left '|'
%left '^'
%left '&'
%left "==" "!="
%left '<' '>' "<=" ">="
%left "<=>"
%left "<<" ">>"
%left '+' '-'
%left '*'
%left '/'
%left '%'
%right '!' "++" "--"
%left '.'
%left '[' '('

%type<CompoundStatement> statements toplevelstatements
%type<std::string> IDENTIFIER
%type<int> INTLITERAL
%type<Expression> expression
%type<std::vector<Expression>> exprList
%type<Declaration> decl
%type<std::vector<Declaration>> decllist declaration
%type<Qualifier> qualifier
%type<StorageSpecifier> storageSpecifier
%type<Type> type
%type<IfElse> ifStatement
%type<Statement> statement forStatement ifElse whileStatement doWhile strongStatement
%%

toplevelstatements: statements { std::move($1.statements.begin(), $1.statements.end(), std::back_inserter(context.topLevelStatements)); }

statements: statements statement { $1.statements.emplace_back(M($2)); $$ = M($1); }
          | %empty               { $$ = {}; }

statement: expression ';'     { $$ = Statement{M($1)}; }
         | '{' statements '}' { $$ = M($2); }
         | ifStatement        { $$ = M($1); }
         | ifElse             { $$ = M($1); }
         | whileStatement     { $$ = M($1); }
         | doWhile            { $$ = M($1); }
         | forStatement       { $$ = M($1); }
         | strongStatement    { $$ = M($1); }
         | ';'                { $$ = NoOp{}; }

expression: expression '.' IDENTIFIER     { $$ = MemberAccess{std::make_shared<Expression>(M($1)), M($3)}; }
          | declaration                   { $$ = {M($1)}; }
          | expression '=' expression     { $$ = Assignment{std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | IDENTIFIER                    { $$ = Expression{Identifier{M($1)}}; }
          | "++" expression               { $$ = Preincrement {std::make_shared<Expression>(M($2))}; }
          | "--" expression %prec "++"    { $$ = Predecrement {std::make_shared<Expression>(M($2))}; }
          | expression "++" %prec "--"    { $$ = Postincrement{std::make_shared<Expression>(M($1))}; }
          | expression "--"               { $$ = Postdecrement{std::make_shared<Expression>(M($1))}; }
          | '!' expression                { $$ = UnaryNot   {std::make_shared<Expression>(M($2))}; }
          | '&' expression %prec '!'      { $$ = AddressOf  {std::make_shared<Expression>(M($2))}; }
          | '*' expression %prec '!'      { $$ = Dereference{std::make_shared<Expression>(M($2))}; }
          | '-' expression %prec '!'      { $$ = UnaryMinus {std::make_shared<Expression>(M($2))}; }
          | expression '+'  expression    { $$ = Addition      {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '-'  expression    { $$ = Subtraction   {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '*'  expression    { $$ = Multiplication{std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '/'  expression    { $$ = Division      {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '%'  expression    { $$ = Modulus       {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '&'  expression    { $$ = BitwiseAnd    {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '|'  expression    { $$ = BitwiseOr     {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression '^'  expression    { $$ = BitwiseXor    {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression "&&" expression    { $$ = LogicalAnd    {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression "||" expression    { $$ = LogicalOr     {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression "==" expression    { $$ = Equals        {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | expression "!=" expression    { $$ = NotEquals     {std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | '(' expression ')'            { $$ = M($2); }
        //| expression expression         { CompoundExpression call{CompoundExpression::Type::FunctionCall}; call.parameters.emplace_back(M($2)); call.parameters.emplace_back(M($1)); $$ = M(call); }
          | expression '<' exprList '>'   { $$ = TemplateInst{std::make_shared<Expression>(M($1)), M($3)}; }
          | expression '(' exprList ')'   { $$ = FunctionCall{std::make_shared<Expression>(M($1)), M($3)}; }
          | "return" expression           { $$ = Return{std::make_shared<Expression>(M($2))}; }
        //| expression ',' expression     { $$ = CommaExpression{std::make_shared<Expression>(M($1)), std::make_shared<Expression>(M($3))}; }
          | INTLITERAL                    { $$ = {M($1)}; } 

exprList: expression              { $$ = {};    $$.emplace_back(M($1)); }
        | exprList ',' expression { $$ = M($1); $$.emplace_back(M($3)); }
        | %empty                  { $$ = {}; }

qualifier: %empty               { $$ = Qualifier{Qualifier::Type::None}; }
         | "const"    qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Const))}; }
         | "volatile" qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Volatile))}; }
         | "mutable"  qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Mutable))}; }
         | "override" qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Override))}; }

storageSpecifier: %empty                          { $$ = StorageSpecifier{StorageSpecifier::Type::None}; }
                | "static"       storageSpecifier { $$ = StorageSpecifier{static_cast<StorageSpecifier::Type>(static_cast<int>($2.type) | static_cast<int>(StorageSpecifier::Type::Static))}; }
                | "thread_local" storageSpecifier { $$ = StorageSpecifier{static_cast<StorageSpecifier::Type>(static_cast<int>($2.type) | static_cast<int>(StorageSpecifier::Type::ThreadLocal))}; }
                | "register"     storageSpecifier { $$ = StorageSpecifier{static_cast<StorageSpecifier::Type>(static_cast<int>($2.type) | static_cast<int>(StorageSpecifier::Type::Register))}; }
                | "extern"       storageSpecifier { $$ = StorageSpecifier{static_cast<StorageSpecifier::Type>(static_cast<int>($2.type) | static_cast<int>(StorageSpecifier::Type::Extern))}; }

type: storageSpecifier expression qualifier { $$ = Type{std::make_unique<Expression>(M($2)), M($3), M($1)}; }

declaration: type decllist { $$ = M($2); auto tp = std::make_shared<Expression>(M($1)); for(auto &decl: $$) decl.type = tp; }

decllist: decl              { $$.emplace_back(M($1)); }
        | decllist ',' decl { $$ = M($1); $$.emplace_back(M($3)); }

decl: IDENTIFIER '=' expression   { $$ = { nullptr, M($1) }; $$.arguments.emplace_back(M($3)); }
    | IDENTIFIER '{' exprList '}' { $$ = { nullptr, M($1), M($3)}; }
    | IDENTIFIER '(' exprList ')' { $$ = { nullptr, M($1), M($3)}; }
    | IDENTIFIER                  { $$ = { nullptr, M($1)}; }

/*uninitializedDeclaration: type IDENTIFIER                          { $$ = Declaration{std::make_shared<Expression>(M($1)), M($2), nullptr}; }

intializedDeclaration: uninitializedDeclaration '{' expression '}' { $1.valueAssigned = std::make_shared<Expression>(M($3)); $$ = M($1); }
                     | uninitializedDeclaration '=' expression     { $1.valueAssigned = std::make_shared<Expression>(M($3)); $$ = M($1); }
                     | IDENTIFIER '{' expression '}'               { $$ = Declaration{nullptr, M($1), std::make_shared<Expression>(M($3))}; }

declaration: uninitializedDeclaration { $$ = M($1); }
           | intializedDeclaration    { $$ = M($1); }*/

ifStatement:      "if"    '(' expression ')' statement                               { $$ = IfElse{M($3), std::make_shared<Statement>(M($5))}; }
ifElse:           ifStatement "else" statement                                       { $1.notTaken = std::make_shared<Statement>(M($3)); $$ = M($1); }
forStatement:     "for"   '(' expression ';' expression ';' expression ')' statement { $$ = For{M($3), M($5), M($7), std::make_shared<Statement>($9)}; }
whileStatement:   "while" '(' expression ')' statement                               { $$ = For{NoOp{}, M($3), NoOp{}, std::make_shared<Statement>($5)}; }
doWhile:          "do" statement "while" '(' expression ')' ';'                      { $$ = DoWhile{std::make_shared<Statement>(M($2)), M($5)}; }
strongStatement:  "strong" IDENTIFIER IDENTIFIER statement                           { $$ = Strong{M($2), M($3), std::make_shared<Statement>(M($4))}; }                                       
%%

yy::Parser::symbol_type yy::yylex(LexerContext &context) {
  char const *anchor = context.cursor;
  context.loc.step();
  //char const *YYMARKER;
  auto s = [&](auto func, auto&&... params) {
    context.loc.columns(context.cursor - anchor);
    return func(params..., context.loc);
  };

  %{
    re2c:yyfill:enable   = 0;
    re2c:define:YYCTYPE  = "char";
    re2c:define:YYCURSOR = "context.cursor";

    "return"  { return s(Parser::make_RETURN); }
    "while"   { return s(Parser::make_WHILE); }
    "for"     { return s(Parser::make_FOR); }
    "do"      { return s(Parser::make_DO); }
    "if"      { return s(Parser::make_IF); }
    "else"    { return s(Parser::make_ELSE); }
    "strong"  { return s(Parser::make_STRONG); }
    //"val" | "value" | "local" { return s(Parser::make_VALUE); }
    //"ref" | "reference"       { return s(Parser::make_REFERENCE); }
    //"ptr" | "pointer"         { return s(Parser::make_POINTER); }

    "&&" | "and" { return s(Parser::make_AND); }
    "||" | "or"  { return s(Parser::make_OR); }
    "++" { return s(Parser::make_INC); }
    "--" { return s(Parser::make_DEC); }

    "!=" { return s(Parser::make_NE); }
    "==" { return s(Parser::make_EQ); }
    "+=" { return s(Parser::make_PLEQ); }
    "-=" { return s(Parser::make_MIEQ); }

    [a-zA-Z][a-zA-Z_0-9]* { return s(Parser::make_IDENTIFIER, std::string{anchor, context.cursor}); }
    [0-9]+                { return s(Parser::make_INTLITERAL, std::stoi(std::string{anchor, context.cursor})); }

    "\000"          { return s(Parser::make_END); }
    "\r\n" | [\r\n] { context.loc.lines();   return yylex(context); }
    "//" [^\r\n]*   {                        return yylex(context); }
    [\t\v\b\f ]     { context.loc.columns(); return yylex(context); }
    .               { return s([](auto...s){return Parser::symbol_type(s...);}, Parser::token_type(context.cursor[-1]&0xFF)); } // Single character tokens
  %}
}

void yy::Parser::error(location_type const &l, std::string const &m) {
  std::cerr << (l.begin.filename ? l.begin.filename->c_str() : "(undefined)");
  std::cerr << ':' << l.begin.line << ":" << l.begin.column << '-' << l.end.column << ": " << m << "\n";
}

#include <fstream>

int main(int argc, char **argv) {
  LexerContext context;
  for(int i = 1; i < argc; ++i){
    std::string filename{argv[i]};
    std::ifstream file{filename};
    std::string buffer{std::istreambuf_iterator<char>{file}, {}};
    context.cursor = buffer.c_str();
    context.loc.begin.filename = &filename;
    context.loc.end.filename = &filename;

    yy::Parser parser{context};
    parser.parse();

    for(auto &s : context.topLevelStatements) {
      std::cout << "Statement: " << s << "\n";
    }
  }
}
