#include "Meter/Token.hh"

#include <array>

// That I have to do this is why I'm making this to begin with
// #define gen(arg) case arg: return next(arg);
// #define gen4(arg) gen(arg) gen(arg + 1) gen(arg + 2) gen(arg + 3)
// #define gen16(arg) gen4(arg) gen4(arg + 4) gen4(arg + 8) gen4(arg + 12)
// #define gen64(arg) gen16(arg) gen16(arg + 16) gen16(arg + 32) gen16(arg + 48)
// #define gen256 gen64(0) gen64(64) gen64(128) gen64(192)

namespace {
  void skipLeadingSpaces(char const *&s) {
    for(;*s && std::isspace(*s);++s);
  }

  constexpr int consumeUTF8(unsigned char c) {
    constexpr auto arr = []() constexpr {
      std::array<char, 256> arr{};
      for(int i = 0; i < 256; ++ i) {
        arr[i] = [&]() constexpr {
               if ((i & 0x80) == 0x00) return 1;
          else if ((i & 0xe0) == 0xc0) return 2;
          else if ((i & 0xf0) == 0xe0) return 3;
          else if ((i & 0xf8) == 0xf0) return 4;
          return 1; // Technically an error but whatever
        }();
      }
      return arr;
    }();
    return arr[c];
  }

  constexpr char consumeIdentifierChar(unsigned char c) {
    constexpr auto arr = []() constexpr {
      std::array<char, 256> arr{};
      for(int i = 0; i < 256; ++ i) {
        arr[i] = [&]() constexpr {
          if('a' <= i && i <= 'z') return 1;
          if('A' <= i && i <= 'Z') return 1;
          if('0' <= i && i <= '9') return 1;
          if(i == '_') return 1;
          // Allow any unicode char
          if(i & 0x80)
            return consumeUTF8(i);
          // Don't allow anything else
          return 0;
        }();
      }
      return arr;
    }();
    return arr[c];
  }
}

Meter::Tokens::Token Meter::Tokens::consumeToken(char const *&s) {
  auto tok = [&]() -> Meter::Tokens::Token {
    retry:
    skipLeadingSpaces(s);
    using namespace Meter::Tokens::Literals;
    if(!*s) return Meter::Tokens::EndOfFile;
    switch(*s) {
      case '-': {
        switch(s[1]) {
          case '-': return "--"_token;
          case '>': return "->"_token;
          case '=': return "-="_token;
          default: return "-"_token;
        }
      }
      case '<': {
        switch(s[1]) {
          case '<': return "<<"_token;
          case '=': {
            if(s[2] == '>') return "<=>"_token;
            return "<="_token;
          }
          default: return "<"_token;
        }
      }
      case '>': {
        switch(s[1]) {
          case '>': return ">>"_token;
          case '=': return ">="_token;
          default: return ">"_token;
        }
      }
      case '&': {
        switch(s[1]) {
          case '&': return "&&"_token;
          case '=': return "&="_token;
          default: return "&"_token;
        }
      }
      case '|': {
        switch(s[1]) {
          case '|': return "||"_token;
          case '=': return "|="_token;
          default: return "|"_token;
        }
      }
      case '^': {
        if(s[1] == '=') return "^="_token;
        return "^"_token;
      }
      case '=': {
        if(s[1] == '=') return "=="_token;
        return "="_token;
      }
      case '!': {
        if(s[1] == '=') return "!="_token;
        return "!"_token;
      }
      case '+': {
        switch(s[1]) {
          case '+': return "++"_token;
          case '=': return "+="_token;
          default: return "+"_token;
        }
      }
      case '/': {
        switch(s[1]) {
          case '=': return "/="_token;
          case '/': {
            s += 2;
            for(;;) {
              switch(*s) {
                case '\0': return Tokens::EndOfFile;
                case '\n': goto retry;
                default: {
                  auto consume = consumeUTF8(*s);
                  for(int i = 1; i < consume; ++ i)
                    if(!s[i]) return Tokens::EndOfFile;
                  s += consume;
                }
              }
            }
          }
          case '*': {
            s += 2;
            for(;;) {
              switch(*s) {
                case '\0': return Tokens::EndOfFile;
                case '*': {
                  if(s[1] == '/') {
                    s += 2;
                    goto retry;
                  }
                  ++s;
                  break;
                }
                default: {
                  auto consume = consumeUTF8(*s);
                  for(int i = 1; i < consume; ++ i)
                    if(!s[i]) return Tokens::EndOfFile;
                  s += consume;
                }
              }
            }
          }
          default: return "/"_token;
        }
      }
      case '*': {
        if(s[1] == '=') return "*="_token;
        return "*"_token;
      }
      case '%': {
        if(s[1] == '=') return "%="_token;
        return "%"_token;
      }
      case '.': {
        switch(s[1]) {
          /*
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':*/
            goto parsenum;
          default:
            return "."_token;
        }
      }
      case '~': return "~"_token;
      case '?': return "?"_token;
      case ';': return ";"_token;
      case ':': return ":"_token;
      case ',': return ","_token;
      case '(': return "("_token;
      case ')': return ")"_token;
      case '{': return "{"_token;
      case '}': return "}"_token;
      case '[': return "["_token;
      case ']': return "]"_token;
      //String literal?
      case '"': { // Shit.
        Tokens::Literal l;
        for(auto sz = 1ull; s[sz];) {
          if(s[sz] == '"') {
            l.length = ++sz;
            l.value = std::string_view{&s[1], sz-2};
            return l;
          }
          if(s[sz] == '\\') ++sz;
          auto consume = consumeUTF8(s[sz]);
          for(int i = 0; i < consume; ++ i)
            if(!s[sz + i]) return Tokens::EndOfFile;
          sz += consume;
        }
        // Unmatched '"'
        return Tokens::EndOfFile;
      }
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9': { // It's some kind of number
        parsenum:
        Tokens::Number num{};
        auto len = 0ull;
        for(; '0' <= s[len] && s[len] <= '9'; ++len);
        num.length = len;
        num.value = std::string_view{s, len};
        return num;
      }
      case 'f': {
        if(s[1] == 'o' && s[2] == 'r') return "for"_token;
        goto ident;
      }
      case 'i': {
        if(s[1] == 'f') return "if"_token;
        goto ident;
      }
      case 'e': {
        if(s[1] == 'l' && s[2] == 's' && s[3] == 'e') return "else"_token;
        goto ident;
      }
      case 'w': {
        if(s[1] == 'h' && s[2] == 'i' && s[3] == 'l' && s[4] == 'e')
          return "while"_token;
        goto ident;
      }
      default: { // Probably an identifier
        ident:
        auto len = 0ull;
        for(;;) {
          auto consume = consumeIdentifierChar(s[len]);
          if(!consume) break;
          for(int i = 0; i < consume; ++ i)
            if(!s[len + i]) return Tokens::EndOfFile;
          len += consume;
        }
        if(!len) {
          return Tokens::EndOfFile;
        }
        else {
          Tokens::Identifier i;
          i.length = len;
          i.value = std::string_view{s, len};
          return i;
        }
      }
    }
  }();
  std::visit([&](auto token){
    return s += token.length;
  }, tok);
  return tok;
}
