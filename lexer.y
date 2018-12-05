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
    Static   = 1 << 2,
    Mutable  = 1 << 3,
    Override = 1 << 4,
  };
  Type type;
};

struct Expression;

struct Type {
  std::shared_ptr<Expression> type;
  Qualifier qual;
};

struct Declaration {
  std::shared_ptr<Expression> type;
  Identifier ident;
  std::shared_ptr<Expression> valueAssigned;
};

struct CompoundExpression {
  enum struct Type {
    Addition,        // lhs + rhs
    Subtraction,     // lhs - rhs
    Multiplication,  // lhs * rhs
    Division,        // lhs / rhs
    UnaryMinus,      // -rhs
    UnaryNot,        // !expr
    Equals,          // lhs == rhs
    NotEquals,       // lhs != rhs
    LogicalAnd,      // lhs && rhs
    LogicalOr,       // lhs || rhs
    BitwiseAnd,      // lhs & rhs
    BitwiseOr,       // lhs | rhs
    BitwiseXor,      // lhs ^ rhs
    AddressOf,       // &rhs
    Dereference,     // *rhs
    Assignment,      // lhs = rhs
    FunctionCall,    // func(args, ...)
    Paren,           // ( expr; ... )
    Return,          // return rhs
    MemberAccess,    // lhs.rhs
    Preincrement,    // ++expr
    Postincrement,   // expr++
    Predecrement,    // --expr
    Postdecrement,   // expr--
    CommaExpression, // lhs, rhs
  };

  Type type;
  std::vector<Expression> parameters;
};

struct NoOp { };

using ExprT = std::variant<Identifier
                         , NoOp
                         , CompoundExpression
                         , Type
                         , Declaration
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
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Static)) os << " static";
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Mutable)) os << " mutable";
  if(static_cast<int>(q.type) & static_cast<int>(Qualifier::Type::Override)) os << " override";
  return os;
}

std::ostream &operator<<(std::ostream &os, ExprT const &ex) {
  std::visit([&os](auto ex) {
    if constexpr(std::is_same_v<decltype(ex), Identifier>) { os << '[' << ex << ']'; }
    else if constexpr(std::is_same_v<decltype(ex), NoOp>) { }
    else if constexpr(std::is_same_v<decltype(ex), CompoundExpression>) { os << (int)ex.type << "{"; for(auto &e:ex.parameters) os << e; os << '}'; }
    else if constexpr(std::is_same_v<decltype(ex), Type>) { os << *ex.type << ex.qual; }
    else if constexpr(std::is_same_v<decltype(ex), Declaration>) {
      if(ex.valueAssigned) os << "decl: " << *ex.type << " " << ex.ident << '{' << *ex.valueAssigned << '}';
      else                 os << "decl: " << *ex.type << " " << ex.ident;
    }
    else if constexpr(std::is_same_v<decltype(ex), std::string>) { os << '"' << ex << '"'; }
    else if constexpr(std::is_same_v<decltype(ex), int>) { os << ex; }
    else if constexpr(std::is_same_v<decltype(ex), unsigned>) { os << ex << 'u'; }
    else if constexpr(std::is_same_v<decltype(ex), long long>) { os << ex << "ll"; }
    else if constexpr(std::is_same_v<decltype(ex), unsigned long long>) { os << ex << "ull"; }
    else if constexpr(std::is_same_v<decltype(ex), float>) { os << ex << 'f'; }
    else if constexpr(std::is_same_v<decltype(ex), double>) { os << ex << 'd'; }
    else { os << "Unknown expression"; }
  }, ex);
  return os;
}

std::ostream &operator<<(std::ostream &os, StatementT const &stmt) {
  std::visit([&os](auto st){
    if constexpr(std::is_same_v<decltype(st), CompoundStatement>) { os << '{'; for(auto &s: st.statements) os << s; os << '}'; }
    else if constexpr(std::is_same_v<decltype(st), Strong>) { os << "strong " << st.typeType << " type " << st.identifier << ": " << *st.contents; }
    else if constexpr(std::is_same_v<decltype(st), For>) { os << "for(" << st.initialization << ", " << st.condition << ", " << st.loopEnd << ") " << *st.loopBody; }
    else if constexpr(std::is_same_v<decltype(st), DoWhile>) { os << "do " << st.loopBody << " while (" << st.condition << ')'; }
    else if constexpr(std::is_same_v<decltype(st), IfElse>) { os << "if(" << st.condition << ") " << *st.taken << " else " << *st.notTaken; }
    else if constexpr(std::is_same_v<decltype(st), Expression>) { os << st; }
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
%token CONST "const" VOLATILE "volatile" STATIC "static" MUTABLE "mutable" OVERRIDE "override"
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
%type<Declaration> declaration intializedDeclaration uninitializedDeclaration
%type<CompoundExpression> cexpr
%type<Qualifier> qualifier
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

expression: expression '.' expression     { CompoundExpression exp{CompoundExpression::Type::MemberAccess}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | declaration                   { $$ = Expression{M($1)}; }
          | expression '=' expression     { CompoundExpression exp{CompoundExpression::Type::Assignment}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | IDENTIFIER                    { $$ = Expression{Identifier{M($1)}}; }
          | "++" expression               { CompoundExpression exp{CompoundExpression::Type::Preincrement}; exp.parameters.emplace_back(M($2)); $$ = M(exp); }
          | "--" expression %prec "++"    { CompoundExpression exp{CompoundExpression::Type::Predecrement}; exp.parameters.emplace_back(M($2)); $$ = M(exp); }
          | expression "++" %prec "--"    { CompoundExpression exp{CompoundExpression::Type::Postincrement}; exp.parameters.emplace_back(M($1)); $$ = M(exp); }
          | expression "--"               { CompoundExpression exp{CompoundExpression::Type::Postdecrement}; exp.parameters.emplace_back(M($1)); $$ = M(exp); }
          | '!' expression                { CompoundExpression exp{CompoundExpression::Type::UnaryNot}; exp.parameters.emplace_back(M($2)); $$ = M(exp); }
          | '&' expression %prec '!'      { CompoundExpression exp{CompoundExpression::Type::AddressOf}; exp.parameters.emplace_back(M($2)); $$ = M(exp); }
          | '*' expression %prec '!'      { CompoundExpression exp{CompoundExpression::Type::Dereference}; exp.parameters.emplace_back(M($2)); $$ = M(exp); }
          | '-' expression %prec '!'      { CompoundExpression exp{CompoundExpression::Type::UnaryMinus}; exp.parameters.emplace_back(M($2)); $$ = M(exp); }
          | expression '+' expression     { CompoundExpression exp{CompoundExpression::Type::Addition}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression '-' expression     { CompoundExpression exp{CompoundExpression::Type::Subtraction}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression '*' expression     { CompoundExpression exp{CompoundExpression::Type::Multiplication}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression '/' expression     { CompoundExpression exp{CompoundExpression::Type::Division}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression '&' expression     { CompoundExpression exp{CompoundExpression::Type::BitwiseAnd}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression '|' expression     { CompoundExpression exp{CompoundExpression::Type::BitwiseOr}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression '^' expression     { CompoundExpression exp{CompoundExpression::Type::BitwiseXor}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression "&&" expression    { CompoundExpression exp{CompoundExpression::Type::LogicalAnd}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression "||" expression    { CompoundExpression exp{CompoundExpression::Type::LogicalOr}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | expression "!=" expression    { CompoundExpression exp{CompoundExpression::Type::NotEquals}; exp.parameters.emplace_back(M($1)); exp.parameters.emplace_back(M($3)); $$ = M(exp); }
          | '(' expression ')'            { CompoundExpression paren{CompoundExpression::Type::Paren}; paren.parameters.emplace_back(M($2)); $$ = M(paren); }
        //| expression expression         { CompoundExpression call{CompoundExpression::Type::FunctionCall}; call.parameters.emplace_back(M($2)); call.parameters.emplace_back(M($1)); $$ = M(call); }
          | expression '<' expression '>' { CompoundExpression call{CompoundExpression::Type::FunctionCall}; call.parameters.emplace_back(M($1)); call.parameters.emplace_back(M($3)); $$ = M(call); }
          | expression '(' expression ')' { CompoundExpression call{CompoundExpression::Type::FunctionCall}; call.parameters.emplace_back(M($1)); call.parameters.emplace_back(M($3)); $$ = M(call); }
          | "return" expression           { CompoundExpression ret{CompoundExpression::Type::Return}; ret.parameters.emplace_back(M($2)); $$ = M(ret); }
          | cexpr                         { $$ = {M($1)}; }
          | INTLITERAL                    { $$ = {M($1)}; } 

cexpr: expression ',' expression                                   { CompoundExpression cexpr{CompoundExpression::Type::CommaExpression}; cexpr.parameters.emplace_back(M($1)); cexpr.parameters.emplace_back(M($3)); $$ = M(cexpr); }

qualifier: %empty               { $$ = Qualifier{Qualifier::Type::None}; }
         | "const"    qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Const))}; }
         | "volatile" qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Volatile))}; }
         | "static"   qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Static))}; }
         | "mutable"  qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Mutable))}; }
         | "override" qualifier { $$ = Qualifier{static_cast<Qualifier::Type>(static_cast<int>($2.type) | static_cast<int>(Qualifier::Type::Override))}; }

type: expression qualifier { $$ = Type{std::make_unique<Expression>(M($1)), M($2)}; }

uninitializedDeclaration: type IDENTIFIER                          { $$ = Declaration{std::make_shared<Expression>(M($1)), M($2), nullptr}; }

intializedDeclaration: uninitializedDeclaration '{' expression '}' { $1.valueAssigned = std::make_shared<Expression>(M($3)); $$ = M($1); }
                     | uninitializedDeclaration '=' expression     { $1.valueAssigned = std::make_shared<Expression>(M($3)); $$ = M($1); }
                     | IDENTIFIER '{' expression '}'               { $$ = Declaration{nullptr, M($1), std::make_shared<Expression>(M($3))}; }

declaration: uninitializedDeclaration { $$ = M($1); }
           | intializedDeclaration    { $$ = M($1); }

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
    "strong" { return s(Parser::make_STRONG); }
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
