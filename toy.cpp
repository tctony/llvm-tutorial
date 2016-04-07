#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>

//= Lexer
enum Token {
  tok_eof = -1,
  tok_def = -2,
  tok_extern = -3,
  tok_identifier = -4,
  tok_number = -5
};

static std::string IdentifierStr;
static double NumVal;

static int gettok() {
  static int LastChar = ' ';

  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;

    if (IdentifierStr == "def") return tok_def;
    if (IdentifierStr == "extern") return tok_extern;
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.')  {
    IdentifierStr = "";
    do {
      IdentifierStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(IdentifierStr.c_str(), 0);
    return tok_number;
  }

  if (LastChar == '#') {
    do LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  if (LastChar == EOF)
    return tok_eof;

  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

static void printtok(int tok) {
  std::string str;
  switch (tok) {
  case tok_eof: str = "eof"; break;
  case tok_def: str = "def"; break;
  case tok_extern: str = "extern"; break;
  case tok_identifier: str = IdentifierStr; break;
  case tok_number: str = IdentifierStr; break;
  default: str = tok; break;
  }

  fprintf(stdout, "%s\n", str.c_str());
}

//= AST
namespace {
  class ExprAST {
  public:
    virtual ~ExprAST() {}
  };

  class NumberExprAST : public ExprAST {
  public:
    NumberExprAST(double val) {}
  };

  class VariableExprAST : public ExprAST {
    std::string Name;
  public:
    VariableExprAST(const std::string &name) : Name(name) {}
  };

  class BinaryExprAST : public ExprAST {
  public:
    BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs) {}
  };

  class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<ExprAST *> Args;
  public:
    CallExprAST(const std::string &callee, std::vector<ExprAST *> &args)
      : Callee(callee), Args(args) {}
  };

  class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
  public:
    PrototypeAST(const std::string &name, const std::vector<std::string> &args)
      : Name(name), Args(args) {}
  };

  class FunctionAST {
  public:
    FunctionAST(PrototypeAST *proto, ExprAST *body) {}
  };
}

//= Parser
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

//== Binary Operator Precedence
static std::map<char, int> BinopPrecedence;

static void initBinopPrecedence() {
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;
}

static int getTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0) return -1;
  return TokPrec;
}

//== Error
ExprAST *Error(const char *str) { fprintf(stderr, "Error: %s\n", str); return 0; }
PrototypeAST *ErrorP(const char *str) { ErrorP(str); return 0; }
FunctionAST *ErrorF(const char *str) { Error(str); return 0; }

static ExprAST *ParseExpression();

//== identifierexpr
//==   ::= identifier
//==   ::= identifier '(' expression* ')'
static ExprAST *ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier

  if (CurTok != '(')
    return new VariableExprAST(IdName);

  getNextToken(); // eat (
  std::vector<ExprAST *> Args;
  if (CurTok != ')') {
    while (1) {
      ExprAST *Arg = ParseExpression();
      if (!Arg) return 0;
      Args.push_back(Arg);

      if (CurTok == ')') break;

      if (CurTok == ',')
        return Error("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  getNextToken(); // eat )

  return new CallExprAST(IdName, Args);
}

//== numberexpr ::= number
static ExprAST *ParseNumberExpr() {
  ExprAST *Result = new NumberExprAST(NumVal);
  getNextToken();
  return Result;
}

//== parenexpr ::= '(' expression ')'
static ExprAST *ParseParenExpr() {
  getNextToken(); // eat (
  ExprAST *V =  ParseExpression();
  if (!V) return 0;

  if (CurTok != ')')
    return Error("expected ')'");
  getNextToken(); // eat )
  return V;
}

//== primary
//==   ::= identifierexpr
//==   ::= numberexpr
//==   ::= parenexpr
static ExprAST *ParsePrimary() {
  switch (CurTok) {
  case tok_identifier: return ParseIdentifierExpr();
  case tok_number:     return ParseNumberExpr();
  case '(':            return ParseParenExpr();
  default:             return Error("unknown token when expecting an expresion");
  }
}

//== binoprhs
//==   ::= ('+' primary)*
static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
  while (1) {
    int TokPrec = getTokPrecedence();

    if (TokPrec < ExprPrec)
      return LHS;

    int BinOp = CurTok;
    getNextToken(); // eat binop

    ExprAST *RHS = ParsePrimary();
    if (!RHS) return 0;

    int NextPrec = getTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec+1, RHS);
      if (!RHS) return 0;
    }

    LHS = new BinaryExprAST(BinOp, LHS, RHS);
  }
}

//== expression
//==   ::= primary binoprhs
static ExprAST *ParseExpression() {
  ExprAST *LHS = ParsePrimary();
  if (!LHS) return 0;

  return ParseBinOpRHS(0, LHS);
}

//== prototype
//==   ::= id '(' id* ')'
static PrototypeAST *ParsePrototype() {
  if (CurTok != tok_identifier)
    return ErrorP("expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return ErrorP("expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);

  if (CurTok != ')')
    return ErrorP("expected ')' in prototype");

  getNextToken(); // eat )

  return new PrototypeAST(FnName, ArgNames);
}

//== definition ::= 'def' prototype expression
static FunctionAST *ParseDefinition() {
  getNextToken(); // eat def
  PrototypeAST *Proto = ParsePrototype();
  if (!Proto) return 0;

  if (ExprAST *E = ParseExpression())
    return new FunctionAST(Proto, E);

  return 0;
}

//== toplevelexpr ::= expression
static FunctionAST *ParseTopLevelExpr() {
  if (ExprAST *E = ParseExpression()) {
    PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
    return new FunctionAST(Proto, E);
  }
  return 0;
}

//== external ::= 'extern' prototype
static PrototypeAST *ParseExtern() {
  getNextToken(); // eat extern
  return ParsePrototype();
}

//= Top Level Parsing
static void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    getNextToken(); // skip current token
  }
}

static void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
  } else {
    getNextToken(); // skip current token
  }
}

static void HandleTopLevelExpression() {
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expression\n");
  } else {
    getNextToken(); //  skip current token
  }
}

//== top ::= definition | external | expression | ';'
static void MainLoop() {
  while (1) {
    fprintf(stdout, "ready> ");

  ready:
    switch(CurTok) {
    case tok_def:     HandleDefinition(); break;
    case tok_extern:  HandleExtern(); break;
    default:          HandleTopLevelExpression(); break;
    case ';':         getNextToken(); break;
    case tok_eof:     return;
    }

    if (CurTok == ';') goto ready;
  }
}


int main() {
  initBinopPrecedence();

  fprintf(stdout, "ready> ");
  getNextToken(); // Prime the first token

  MainLoop();

  return 0;
}

