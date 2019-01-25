#include <iostream>
#include <variant>
#include <memory>
#include <optional>

struct NoOp {
    std::ostream &operator<<(std::ostream &os) const { return os; }
};

using Expression = std::variant<
    struct NoOp
  , struct Identifier
  , struct Addition
  , struct Subtraction
  , struct Multiplication
  , struct Division
  , struct BitwiseAnd
  , struct BitwiseOr
  , struct BitwiseXor
>;

std::ostream &operator<<(std::ostream &os, Expression const &exp);

struct Identifier {
    std::string s;
    std::ostream &operator<<(std::ostream &os) const {
        return os << s;
    }
};

template<char ...op>
struct BinaryOp {
    std::unique_ptr<Expression> lhs, rhs;
    std::ostream &operator<<(std::ostream &os) const;
};
struct Addition:       BinaryOp<'+'> { };
struct Subtraction:    BinaryOp<'-'> { };
struct Multiplication: BinaryOp<'*'> { };
struct Division:       BinaryOp<'/'> { };
struct BitwiseAnd:     BinaryOp<'&'> { };
struct BitwiseOr:      BinaryOp<'|'> { };
struct BitwiseXor:     BinaryOp<'^'> { };

template<char ...op>
std::ostream &BinaryOp<op...>::operator<<(std::ostream &os) const {
    os << *lhs;
    ((os << op), ...);
    return os << *rhs;
}

template<typename T>
T parse(std::istream &is);

template<>
Expression parse<Expression>(std::istream &is) {
    return NoOp{};
}

std::ostream &operator<<(std::ostream &os, Expression const &exp) {
    return os;
}

int main() {
    auto exp = parse<Expression>(std::cin);
    std::cout << exp << '\n';
    Expression add = Addition{std::make_unique<Expression>("Fem"), std::make_unique<Expression>("Sju")};
    std::cout << add;
}
