#include <array>
#include <cctype>

#include "Meter/Tokenizer.hh"

Meter::Tokens::Token Meter::Tokens::consumeToken(Meter::Tokens::TokenizerContext &context) {
  auto contextStart = context;
  if(!context.view.size())
    return Tokens::EndOfFile;
  char const *const tokenStart = &context.view[0];
  auto num = [&] { return static_cast<std::size_t>(&context.view[0] - tokenStart); };
  auto tok = [&] (auto tokenVal) {
    tokenVal.context.view = contextStart.view.substr(0, num());
    tokenVal.context.lineStart = contextStart.rowLoc;
    tokenVal.context.columnStart = contextStart.columnLoc;
    tokenVal.context.columnEnd = context.columnLoc;
    if constexpr(std::is_same_v<decltype(tokenVal.value), std::string_view>) {
      tokenVal.length = num();
      tokenVal.value = tokenVal.context.view;
    }
    return tokenVal;
  };

  decltype(context.view) backup;
  using chr = unsigned char;

  [[maybe_unused]]
  auto YYPEEK    = [&] { return (context.view.empty() ? '\x00' : context.view[0]); };
  [[maybe_unused]]
  auto YYBACKUP  = [&] { backup = context.view; };
  [[maybe_unused]]
  auto YYRESTORE = [&] { context.view = backup; };
  [[maybe_unused]]
  auto YYSKIP    = [&] { if(!context.view.empty()) context.view.remove_prefix(1); };

  auto blockComment = [&]() -> Tokens::Token {
    context.cols(2);
    auto eat = [&](auto self) -> bool {
/*!re2c
  re2c:yyfill:enable = 0;
  re2c:define:YYCTYPE = chr;

  "\x00"                               {                      return true;       }
  "\r\n"|[\r\n]                        { context.rows();      return self(self); }
  "*""/"                               { context.cols(num()); return false;      }
  ([^*\x00\r\n] | ("*" [^/\x00\r\n]))* { context.cols(num()); return self(self); }
*/
    };
    if(eat(eat)) return Tokens::EndOfFile;
    return consumeToken(context);
  };

  [[maybe_unused]]
  auto parseString = [&]() -> Tokens::Token {
    auto eat = [&](auto self) -> bool {
/*!re2c
  re2c:yyfill:enable = 0;
  re2c:define:YYCTYPE = chr;

  "\x00"                    {                      return true;       }
  "\""                      { context.cols(num()); return false;      }
  ([^\"\x00\r\n] | "\\\"")* { context.cols(num()); return self(self); }
*/
    };
    if(eat(eat)) return Tokens::EndOfFile;
    return consumeToken(context);
  };

  // First thing to rewrite with the new language is the tokenizer :^)
/*!re2c
  re2c:yyfill:enable = 0;
  re2c:define:YYCTYPE = chr;
  nonascii = [\u0080-\uffff];
  identstart = [a-zA-Z_$#@] | nonascii;
  identchar = identstart | [0-9];

  "for"           { return tok("for"_token); }
  "if"            { return tok("if"_token); }
  "else"          { return tok("else"_token); }
  "while"         { return tok("while"_token); }
  "struct"        { return tok("struct"_token); }
  "do"            { return tok("do"_token); }
  "return"        { return tok("return"_token); }
  "scope_exit"    { return tok("scope_exit"_token); }
  "scope_fail"    { return tok("scope_fail"_token); }
  "scope_success" { return tok("scope_success"_token); }

  "<<="           { return tok("<<="_token); }
  ">>="           { return tok(">>="_token); }
  "<=>"           { return tok("<=>"_token); }

  "->"            { return tok("->"_token); }
  "<<"            { return tok("<<"_token); }
  ">>"            { return tok(">>"_token); }
  "<="            { return tok("<="_token); }
  ">="            { return tok(">="_token); }
  "&&"            { return tok("&&"_token); }
  "||"            { return tok("||"_token); }
  "=="            { return tok("=="_token); }
  "!="            { return tok("!="_token); }
  "++"            { return tok("++"_token); }
  "--"            { return tok("--"_token); }
  "+="            { return tok("+="_token); }
  "-="            { return tok("-="_token); }
  "/="            { return tok("/="_token); }
  "*="            { return tok("*="_token); }
  "%="            { return tok("%="_token); }
  "%{"            { return tok("%{"_token); }
  "&="            { return tok("&="_token); }
  "^="            { return tok("^="_token); }
  "|="            { return tok("|="_token); }
  "=>"            { return tok("=>"_token); }

  ";"             { return tok(";"_token); }
  "."             { return tok("."_token); }
  ":"             { return tok(":"_token); }
  ","             { return tok(","_token); }
  "+"             { return tok("+"_token); }
  "-"             { return tok("-"_token); }
  "*"             { return tok("*"_token); }
  "/"             { return tok("/"_token); }
  "%"             { return tok("%"_token); }
  "?"             { return tok("?"_token); }
  "("             { return tok("("_token); }
  ")"             { return tok(")"_token); }
  "{"             { return tok("{"_token); }
  "}"             { return tok("}"_token); }
  "["             { return tok("["_token); }
  "]"             { return tok("]"_token); }
  "&"             { return tok("&"_token); }
  "|"             { return tok("|"_token); }
  "^"             { return tok("^"_token); }
  "<"             { return tok("<"_token); }
  ">"             { return tok(">"_token); }
  "~"             { return tok("~"_token); }
  "="             { return tok("="_token); }
  "!"             { return tok("!"_token); }

  // End of file
  "\x00"          { return Meter::Tokens::EndOfFile; }

  // Line comments
  "//" [^\n\x00]* { context.cols(num());       return consumeToken(context); }

  // Whitespaces
  [\t\v\b\f ]+  { context.cols(num());       return consumeToken(context); }

  // Newlines
  "\r\n"|[\r\n] { context.rows();            return consumeToken(context); }

  "/*"          { return blockComment(); }
  "\"" ([^\"\x00\r\n] | "\\\"")* "\"" { return tok(Tokens::Literal{}); }

  identstart identchar * { return tok(Tokens::Identifier{}); }
*/
}
