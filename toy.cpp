#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>

#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

#include <iostream>
#include "llvm/ADT/Triple.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/Support/Host.h"

using namespace llvm;

//= Lexer
enum Token {
  tok_eof = -1,
  tok_def = -2,
  tok_extern = -3,
  tok_identifier = -4,
  tok_number = -5,
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
  tok_for = -9,
  tok_in = -10,
  tok_binary = -11,
  tok_unary = - 12,
  tok_var = -13,
};

static std::string IdentifierStr;
static double NumVal;
struct SourceLocation {
  int Line;
  int Col;
};
static SourceLocation CurLoc;
static SourceLocation LexLoc = { 1, 0 };

static int advance() {
  int LastChar = getchar();

  if (LastChar == '\n' || LastChar == '\r') {
    LexLoc.Line++;
    LexLoc.Col = 0;
  } else {
    LexLoc.Col++;
  }
  return LastChar;
}

static int gettok() {
  static int LastChar = ' ';

  while (isspace(LastChar))
    LastChar = advance();

  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = advance())))
      IdentifierStr += LastChar;

    if (IdentifierStr == "def") return tok_def;
    if (IdentifierStr == "extern") return tok_extern;
    if (IdentifierStr == "if") return tok_if;
    if (IdentifierStr == "then") return tok_then;
    if (IdentifierStr == "else") return tok_else;
    if (IdentifierStr == "for") return tok_for;
    if (IdentifierStr == "in") return tok_in;
    if (IdentifierStr == "binary") return tok_binary;
    if (IdentifierStr == "unary") return tok_unary;
    if (IdentifierStr == "var") return tok_var;
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.')  {
    IdentifierStr = "";
    do {
      IdentifierStr += LastChar;
      LastChar = advance();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(IdentifierStr.c_str(), 0);
    return tok_number;
  }

  if (LastChar == '#') {
    do LastChar = advance();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  if (LastChar == EOF)
    return tok_eof;

  int ThisChar = LastChar;
  LastChar = advance();
  return ThisChar;
}

//= AST
namespace {

  std::ostream &indent(std::ostream &O, int size) {
    return O << std::string(size, ' ');
  }

  class ExprAST {
    SourceLocation Loc;

  public:
    int getLine() const { return Loc.Line; }
    int getCol() const { return Loc.Col; }
    ExprAST(SourceLocation loc = CurLoc) : Loc(loc) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      return out << ':' << getLine() << ':' << getCol() << '\n';
    }
    virtual ~ExprAST() {}
    virtual Value *Codegen() = 0;
  };

  class NumberExprAST : public ExprAST {
    double Val;

  public:
    NumberExprAST(double val) : Val(val) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      return ExprAST::dump(out << Val, ind);
    }
    virtual Value *Codegen();
  };

  class VariableExprAST : public ExprAST {
    std::string Name;

  public:
    VariableExprAST(SourceLocation Loc, const std::string &name)
      : ExprAST(Loc), Name(name) {}
    const std::string &getName() const { return Name; }
    virtual std::ostream &dump(std::ostream &out, int ind) {
      return ExprAST::dump(out << Name, ind);
    }
    virtual Value *Codegen();
  };

  class UnaryExprAST : public ExprAST {
    char Op;
    ExprAST *Operand;

  public:
    UnaryExprAST(char op, ExprAST *operand)
      : Op(op), Operand(operand) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      ExprAST::dump(out << "unary" << Op, ind);
      Operand->dump(out, ind + 1);
      return out;
    }
    virtual Value *Codegen();
  };

  class BinaryExprAST : public ExprAST {
    char Op;
    ExprAST *LHS, *RHS;

  public:
    BinaryExprAST(SourceLocation Loc, char op, ExprAST *lhs, ExprAST *rhs)
      : ExprAST(Loc), Op(op), LHS(lhs), RHS(rhs) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      ExprAST::dump(out << "binary" << Op, ind);
      LHS->dump(indent(out, ind) << "LHS:", ind + 1);
      RHS->dump(indent(out, ind) << "RHS:", ind + 1);
      return out;
    }
    virtual Value *Codegen();
  };

  class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<ExprAST *> Args;

  public:
    CallExprAST(SourceLocation Loc, const std::string &callee,
                std::vector<ExprAST *> &args)
      : ExprAST(Loc), Callee(callee), Args(args) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      ExprAST::dump(out << "call " << Callee, ind);
      for (ExprAST *Arg : Args)
        Arg->dump(indent(out, ind + 1), ind + 1);
      return out;
    }
    virtual Value *Codegen();
  };

  class IfExprAST : public ExprAST {
    ExprAST *Cond, *Then, *Else;

  public:
    IfExprAST(SourceLocation Loc, ExprAST *cond, ExprAST *then, ExprAST *_else)
      : ExprAST(Loc), Cond(cond), Then(then), Else(_else) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      ExprAST::dump(out << "if", ind);
      Cond->dump(indent(out, ind) << "Cond:", ind + 1);
      Then->dump(indent(out, ind) << "Then:", ind + 1);
      Else->dump(indent(out, ind) << "Else:", ind + 1);
      return out;
    }
    virtual Value *Codegen();
  };

  class ForExprAST : public ExprAST {
    std::string VarName;
    ExprAST *Start, *End, *Step, *Body;

  public:
    ForExprAST(const std::string &varname, ExprAST *start, ExprAST *end,
               ExprAST *step, ExprAST *body)
      : VarName(varname), Start(start), End(end), Step(step), Body(body) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      ExprAST::dump(out << "for", ind);
      Start->dump(indent(out, ind) << "Cond:", ind + 1);
      End->dump(indent(out, ind) << "End:", ind + 1);
      Step->dump(indent(out, ind) << "Step:", ind + 1);
      Body->dump(indent(out, ind) << "Body:", ind + 1);
      return out;
    }
    virtual Value *Codegen();
  };

  class VarExprAST : public ExprAST {
    std::vector<std::pair<std::string, ExprAST *> > VarNames;
    ExprAST *Body;

  public:
    VarExprAST(const std::vector<std::pair<std::string, ExprAST *> > &varnames,
               ExprAST *body)
      : VarNames(varnames), Body(body) {}
    virtual std::ostream &dump(std::ostream &out, int ind) {
      ExprAST::dump(out << "var", ind);
      for (const auto &NamedVar : VarNames)
        NamedVar.second->dump(indent(out, ind) << NamedVar.first << ':', ind + 1);
      Body->dump(indent(out, ind) << "Body:", ind + 1);
      return out;
    }
    virtual Value *Codegen();
  };

  class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
    bool isOperator;
    unsigned Precedence;  // Precedence for binary op
    int Line;

  public:
    PrototypeAST(SourceLocation Loc, const std::string &name, const std::vector<std::string> &args,
                 bool isoperator = false, unsigned prec = 0)
      : Name(name), Args(args), isOperator(isoperator), Precedence(prec),
        Line(Loc.Line) {}

    bool isUnaryOp() const { return isOperator && Args.size() == 1; }
    bool isBinaryOp() const { return isOperator && Args.size() == 2; }
    char getOperatorName() const {
      assert(isUnaryOp() || isBinaryOp());
      return Name[Name.size()-1];
    }
    unsigned getBinaryPrecedence() const { return Precedence; }

    Function *Codegen();

    void CreateArgumentAllocas(Function *F);
    const std::vector<std::string> &getArgs() const { return Args; }
  };

  class FunctionAST {
    PrototypeAST *Proto;
    ExprAST *Body;
  public:
    FunctionAST(PrototypeAST *proto, ExprAST *body)
      : Proto(proto), Body(body) {}

    std::ostream &dump(std::ostream &out, int ind) {
      indent(out, ind) << "FunctionAST\n";
      ++ind;
      indent(out, ind) << "Body:";
      return Body ? Body->dump(out, ind) : out << "null\n";
    }

    Function *Codegen();
  };
}

//= Error
ExprAST *Error(const char *str) { fprintf(stderr, "Error: %s\n", str); return 0; }
PrototypeAST *ErrorP(const char *str) { Error(str); return 0; }
FunctionAST *ErrorF(const char *str) { Error(str); return 0; }
Value *ErrorV(const char *str) { Error(str); return 0; }

//= Parser
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

//== Binary Operator Precedence
static std::map<char, int> BinopPrecedence;

static void initBinopPrecedence() {
  BinopPrecedence['='] = 2;
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

static ExprAST *ParseExpression();

//== identifierexpr
//==   ::= identifier
//==   ::= identifier '(' expression* ')'
static ExprAST *ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  SourceLocation LitLoc = CurLoc;

  getNextToken(); // eat identifier

  if (CurTok != '(')
    return new VariableExprAST(LitLoc, IdName);

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

  return new CallExprAST(LitLoc, IdName, Args);
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

//== ifexpr ::= 'if' expression 'then' expression 'else' expression
static ExprAST *ParseIfExpr() {
  SourceLocation IfLoc = CurLoc;

  getNextToken(); // eat if

  ExprAST *Cond = ParseExpression();
  if (!Cond) return 0;

  if (CurTok != tok_then)
    return Error("expected then");
  getNextToken(); // eat then

  ExprAST *Then = ParseExpression();
  if (!Then) return 0;

  if (CurTok != tok_else)
    return Error("expected else");

  getNextToken(); // eat else

  ExprAST *Else = ParseExpression();
  if (!Else) return 0;

  return new IfExprAST(IfLoc, Cond, Then, Else);
}

//== forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static ExprAST *ParseForExpr() {
  getNextToken(); // eat for

  if (CurTok != tok_identifier)
    return Error("expected identifier after for");
  std::string IdName = IdentifierStr;
  getNextToken(); // eat identifier

  if (CurTok != '=')
    return Error("expected '=' after for");
  getNextToken(); // eat =

  ExprAST *Start = ParseExpression();
  if (!Start) return 0;
  if (CurTok != ',')
    return Error("expected ',' after for start value");
  getNextToken();

  ExprAST *End = ParseExpression();
  if (!End) return 0;

  // optional step
  ExprAST *Step = 0;
  if (CurTok == ',') {
    getNextToken();
    Step = ParseExpression();
    if (!Step) return 0;
  }

  if (CurTok != tok_in)
    return Error("expected 'in' after 'for'");
  getNextToken();

  ExprAST *Body = ParseExpression();
  if (!Body) return 0;

  return new ForExprAST(IdName, Start, End, Step, Body);
}

//== varexpr ::= 'var' identifier ('=' expression)?
//==                   (',' identifier ('=' expression)?)* 'in' expression
static ExprAST *ParseVarExpr() {
  getNextToken(); // eat var

  std::vector<std::pair<std::string, ExprAST *> > VarNames;

  // at least one  variable
  if (CurTok != tok_identifier)
    return Error("expected identifier after var");

  while (1) {
    std::string Name = IdentifierStr;
    getNextToken(); // eat Identifier

    ExprAST *Init = 0;
    if (CurTok == '=') {
      getNextToken(); // eat =

      Init = ParseExpression();
      if (Init == 0) return 0;
    }

    VarNames.push_back(std::make_pair(Name, Init));

    if (CurTok != ',') break;
    getNextToken(); // eat ,

    if (CurTok != tok_identifier)
      return Error("expected identifier list after var");
  }

  if (CurTok != tok_in)
    return Error("expected 'in' keyword after 'var'");
  getNextToken();

  ExprAST *Body = ParseExpression();
  if (Body == 0) return 0;

  return new VarExprAST(VarNames, Body);
}

//== primary
//==   ::= identifierexpr
//==   ::= numberexpr
//==   ::= parenexpr
//==   ::= ifexpr
//==   ::= forexpr
//==   ::= varexpr
static ExprAST *ParsePrimary() {
  switch (CurTok) {
  case tok_identifier: return ParseIdentifierExpr();
  case tok_number:     return ParseNumberExpr();
  case '(':            return ParseParenExpr();
  case tok_if:         return ParseIfExpr();
  case tok_for:        return ParseForExpr();
  case tok_var:        return ParseVarExpr();
  default:             return Error("unknown token when expecting an expression");
  }
}

//== unary
//==   ::= primary
//==   ::= '!' unary
static ExprAST *ParseUnary() {
  if (!isascii(CurTok) || CurTok == '(' ||  CurTok == ',')
    return ParsePrimary();

  int Op = CurTok;
  getNextToken();
  if (ExprAST *Operand = ParseUnary())
    return new UnaryExprAST(Op, Operand);

  return 0;
}

//== binoprhs
//==   ::= ('+' unary)*
static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
  while (1) {
    int TokPrec = getTokPrecedence();

    if (TokPrec < ExprPrec)
      return LHS;

    int BinOp = CurTok;
    SourceLocation BinLoc = CurLoc;
    getNextToken(); // eat binop

    ExprAST *RHS = ParseUnary();
    if (!RHS) return 0;

    int NextPrec = getTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec+1, RHS);
      if (!RHS) return 0;
    }

    LHS = new BinaryExprAST(BinLoc, BinOp, LHS, RHS);
  }
}

//== expression
//==   ::= unary binoprhs
static ExprAST *ParseExpression() {
  ExprAST *LHS = ParseUnary();
  if (!LHS) return 0;

  return ParseBinOpRHS(0, LHS);
}

//== prototype
//==   ::= id '(' id* ')'
//==   ::= unary LETTER (id)
//==   ::= binary LETTER number? (id, id)
static PrototypeAST *ParsePrototype() {
  std::string FnName;

  SourceLocation FnLoc = CurLoc;

  unsigned Kind = 0;    // 0 = identifier, 1 = unary, 2 = binary
  unsigned BinaryPrecedence = 30;

  switch(CurTok) {
  default:
    return ErrorP("expected function name in prototype");
  case tok_identifier:
    FnName = IdentifierStr;
    Kind = 0;
    getNextToken();
    break;
  case tok_unary:
    getNextToken();
    if (!isascii(CurTok))
      return ErrorP("expected unary operator");
    FnName = "unary";
    FnName += (char)CurTok;
    Kind = 1;
    getNextToken();
    break;
  case tok_binary:
    getNextToken();
    if (!isascii(CurTok))
      return ErrorP("expected binary operator");
    FnName = "binary";
    FnName += (char)CurTok;
    Kind = 2;
    getNextToken();

    if (CurTok == tok_number) {
      if (NumVal < 1 || NumVal > 100)
        return ErrorP("invalid precedence: must be 1..100");
      BinaryPrecedence = (unsigned)NumVal;
      getNextToken();
    }
    break;
  }

  if (CurTok != '(')
    return ErrorP("expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);

  if (CurTok != ')')
    return ErrorP("expected ')' in prototype");

  getNextToken(); // eat )

  if (Kind && ArgNames.size() != Kind)
    return ErrorP("invalid number of operands for operators");

  return new PrototypeAST(FnLoc, FnName, ArgNames, Kind > 0, BinaryPrecedence);
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
  SourceLocation FnLoc = CurLoc;
  if (ExprAST *E = ParseExpression()) {
    PrototypeAST *Proto = new PrototypeAST(FnLoc, "main", std::vector<std::string>());
    return new FunctionAST(Proto, E);
  }
  return 0;
}

//== external ::= 'extern' prototype
static PrototypeAST *ParseExtern() {
  getNextToken(); // eat extern
  return ParsePrototype();
}

//= Code Generation
static Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, AllocaInst *> NamedValues;
static FunctionPassManager *TheFPM;

static DIBuilder *DBuilder;
struct DebugInfo {
  DICompileUnit TheCU;
  DIType DblTy;
  std::vector<DIScope *> LexicalBlocks;
  std::map<const PrototypeAST *, DIScope> FnScopeMap;

  void emitLocation(ExprAST *AST);
  DIType getDoubleTy();
} KSDbgInfo;

void DebugInfo::emitLocation(ExprAST *AST) {
  if (!AST)
    return Builder.SetCurrentDebugLocation(DebugLoc());
  DIScope *Scope;
  if (LexicalBlocks.empty())
    Scope = &TheCU;
  else
    Scope = LexicalBlocks.back();
  Builder.SetCurrentDebugLocation(DebugLoc::get(AST->getLine(), AST->getCol(), DIScope(*Scope)));
}

DIType DebugInfo::getDoubleTy() {
  if (DblTy.isValid())
    return DblTy;

  DblTy = DBuilder->createBasicType("double", 64, 64, dwarf::DW_ATE_float);
  return DblTy;
}

static DICompositeType CreateFunctionType(unsigned NumArgs, DIFile Unit) {
  SmallVector<Metadata *, 8> EltTys;
  DIType DblTy = KSDbgInfo.getDoubleTy();

  EltTys.push_back(DblTy);

  for (unsigned i = 0, e = NumArgs; i != e; ++i)
    EltTys.push_back(DblTy);

  DITypeArray EltTypeArray = DBuilder->getOrCreateTypeArray(EltTys);
  return DBuilder->createSubroutineType(Unit, EltTypeArray);
}

Value *NumberExprAST::Codegen() {
  KSDbgInfo.emitLocation(this);
  return ConstantFP::get(getGlobalContext(), APFloat(Val));
}

Value *VariableExprAST::Codegen() {
  Value *V = NamedValues[Name];
  if (V == 0) return ErrorV("Unknown variable name");

  KSDbgInfo.emitLocation(this);
  return Builder.CreateLoad(V, Name.c_str());
}

Value *UnaryExprAST::Codegen() {
  Value *OperandV = Operand->Codegen();
  if (OperandV == 0) return 0;

  Function *F = TheModule->getFunction(std::string("unary")+Op);
  if (F == 0)
    return ErrorV("unknown unary operator");

  KSDbgInfo.emitLocation(this);
  return Builder.CreateCall(F, OperandV, "unop");
}

Value *BinaryExprAST::Codegen() {
  KSDbgInfo.emitLocation(this);

  if (Op == '=') {
    VariableExprAST *LHSE = dynamic_cast<VariableExprAST *>(LHS);
    if (!LHSE)
      return ErrorV("destination of '=' must be a variable");

    Value *Val = RHS->Codegen();
    if (!Val) return 0;

    Value *Variable = NamedValues[LHSE->getName()];
    if (!Variable) return 0;

    Builder.CreateStore(Val, Variable);
    return Val;
  }

  Value *L = LHS->Codegen();
  Value *R = RHS->Codegen();
  if (L == 0 || R == 0) return 0;

  switch (Op) {
  case '+': return Builder.CreateFAdd(L, R, "addtmp");
  case '-': return Builder.CreateFSub(L, R, "subtmp");
  case '*': return Builder.CreateFMul(L, R, "multmp");
  case '<':
    L = Builder.CreateFCmpULT(L, R, "cmptmp");
    return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");

  default: break;
  }

  // user defined operators
  Function *F = TheModule->getFunction(std::string("binary")+Op);
  assert(F && "binary operator not found!");

  Value *Ops[2] = { L, R };
  return Builder.CreateCall(F, Ops, "binop");
}

Value *CallExprAST::Codegen() {
  KSDbgInfo.emitLocation(this);

  Function *CalleeF = TheModule->getFunction(Callee);
  if (CalleeF == 0)
    return ErrorV("unknown function referenced");

  if (CalleeF->arg_size() != Args.size())
    return ErrorV("incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->Codegen());
    if (ArgsV.back() == 0) return 0;
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfExprAST::Codegen() {
  KSDbgInfo.emitLocation(this);

  Value *CondV = Cond->Codegen();
  if (CondV == 0) return 0;

  CondV = Builder.CreateFCmpONE(CondV,
                                ConstantFP::get(getGlobalContext(), APFloat(0.0)),
                                "ifcond");

  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", TheFunction);
  BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
  BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");

  Builder.CreateCondBr(CondV, ThenBB, ElseBB);

  // then
  Builder.SetInsertPoint(ThenBB);
  Value *ThenV = Then->Codegen();
  if (ThenV == 0) return 0;
  Builder.CreateBr(MergeBB);
  ThenBB = Builder.GetInsertBlock(); // update then phi

  // else
  TheFunction->getBasicBlockList().push_back(ElseBB);
  Builder.SetInsertPoint(ElseBB);
  Value *ElseV = Else->Codegen();
  if (ElseV == 0) return 0;
  Builder.CreateBr(MergeBB);
  ElseBB = Builder.GetInsertBlock(); // update else phi

  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder.SetInsertPoint(MergeBB);
  PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(getGlobalContext()),
                                  2,
                                  "iftmp");
  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const std::string &VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(getGlobalContext()),
                                             0,
                                             VarName.c_str());
}

Value *ForExprAST::Codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  KSDbgInfo.emitLocation(this);

  Value *StartVal = Start->Codegen();
  if (!StartVal) return 0;

  Builder.CreateStore(StartVal, Alloca);

  BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
  Builder.CreateBr(LoopBB);
  Builder.SetInsertPoint(LoopBB);

  AllocaInst *OldVal = NamedValues[VarName];
  NamedValues[VarName] = OldVal;

  if (Body->Codegen() == 0) return 0;

  Value *StepVal;
  if (Step) {
    StepVal = Step->Codegen();
    if (!StepVal) return 0;
  } else {
    StepVal = ConstantFP::get(getGlobalContext(), APFloat(1.0));
  }

  Value *EndCond = End->Codegen();
  if (!EndCond) return 0;

  Value *CurVar = Builder.CreateLoad(Alloca, VarName.c_str());
  Value *NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
  Builder.CreateStore(NextVar, Alloca);

  EndCond = Builder.CreateFCmpONE(EndCond,
                                  ConstantFP::get(getGlobalContext(), APFloat(0.0)),
                                  "loopcond");

  BasicBlock *AfterBB = BasicBlock::Create(getGlobalContext(),
                                           "afterloop",
                                           TheFunction);

  Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

  Builder.SetInsertPoint(AfterBB);

  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);

  return Constant::getNullValue(Type::getDoubleTy(getGlobalContext()));
}

Value *VarExprAST::Codegen() {
  std::vector<AllocaInst *> OldBindings;

  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second;
    Value *InitVal;
    if (Init) {
      InitVal = Init->Codegen();
      if (InitVal == 0) return 0;
    } else {
      InitVal = ConstantFP::get(getGlobalContext(), APFloat(0.0));
    }

    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    Builder.CreateStore(InitVal, Alloca);

    OldBindings.push_back(NamedValues[VarName]);
    NamedValues[VarName] = Alloca;
  }

  KSDbgInfo.emitLocation(this);

  Value *BodyVal = Body->Codegen();
  if (!BodyVal) return 0;

  for (unsigned i = 0,  e = VarNames.size(); i != e; ++i)
    NamedValues[VarNames[i].first] = OldBindings[i];

  return BodyVal;
}

Function *PrototypeAST::Codegen() {
  std::vector<Type *> Doubles(Args.size(),
                              Type::getDoubleTy(getGlobalContext()));
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()),
                                       Doubles,
                                       false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);

  // conflicted, redefinition or reextern
  if (F->getName() != Name) {
    F->eraseFromParent();
    F = TheModule->getFunction(Name);

    if (!F->empty()) {
      ErrorF("redefinition of function");
      return 0;
    }

    if (F->arg_size() != Args.size()) {
      ErrorF("redefinition of function with different # args");
      return 0;
    }
  }

  unsigned Idx = 0;
  for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size(); ++AI, ++Idx)
    AI->setName(Args[Idx]);

  DIFile Unit = DBuilder->createFile(KSDbgInfo.TheCU.getFilename(),
                                     KSDbgInfo.TheCU.getDirectory());
  DIDescriptor FContext(Unit);
  unsigned LineNo = Line;
  unsigned ScopeLine = Line;
  DISubprogram SP = DBuilder->createFunction(FContext, Name, StringRef(), Unit, LineNo,
                                             CreateFunctionType(Args.size(), Unit), false,
                                             true, ScopeLine, DIDescriptor::FlagPrototyped,
                                             false, F);
  KSDbgInfo.FnScopeMap[this] = SP;

  return F;
}

void PrototypeAST::CreateArgumentAllocas(Function *F) {
  Function::arg_iterator AI = F->arg_begin();
  for (unsigned Idx = 0, e = Args.size(); Idx != e; ++Idx, ++AI) {
    AllocaInst *Alloca = CreateEntryBlockAlloca(F, Args[Idx]);

    DIScope *Scope = KSDbgInfo.LexicalBlocks.back();
    DIFile Unit = DBuilder->createFile(KSDbgInfo.TheCU.getFilename(),
                                       KSDbgInfo.TheCU.getDirectory());
    DIVariable D  =  DBuilder->createLocalVariable(dwarf::DW_TAG_arg_variable,
                                                   *Scope, Args[Idx], Unit, Line,
                                                   KSDbgInfo.getDoubleTy(), Idx);

    Instruction *Call = DBuilder->insertDeclare(Alloca, D,
                                                DBuilder->createExpression(),
                                                Builder.GetInsertBlock());
    Call->setDebugLoc(DebugLoc::get(Line, 0, *Scope));

    Builder.CreateStore(AI, Alloca);
    NamedValues[Args[Idx]] = Alloca;
  }
}

Function *FunctionAST::Codegen() {
  NamedValues.clear();

  Function *TheFunction = Proto->Codegen();
  if (TheFunction == 0)
    return 0;

  KSDbgInfo.LexicalBlocks.push_back(&KSDbgInfo.FnScopeMap[Proto]);
  KSDbgInfo.emitLocation(nullptr);

  if (Proto->isBinaryOp())
    BinopPrecedence[Proto->getOperatorName()] = Proto->getBinaryPrecedence();

  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  Proto->CreateArgumentAllocas(TheFunction);

  if (Value *RetVal = Body->Codegen()) {
    Builder.CreateRet(RetVal);

    KSDbgInfo.LexicalBlocks.pop_back();

    verifyFunction(*TheFunction);

    TheFPM->run(*TheFunction);

    return TheFunction;
  }

  TheFunction->eraseFromParent();

  if (Proto->isBinaryOp())
    BinopPrecedence.erase(Proto->getOperatorName());

  KSDbgInfo.LexicalBlocks.pop_back();

  return 0;
}

//= Top Level Parsing and JIT Driver

static ExecutionEngine *TheExecutionEngine;

static void HandleDefinition() {
  if (FunctionAST *F = ParseDefinition()) {
    if (!F->Codegen()) {
      fprintf(stderr, "error generating code for def");
    }
  } else {
    getNextToken(); // skip current token
  }
}

static void HandleExtern() {
  if (PrototypeAST *P = ParseExtern()) {
    if (!P->Codegen()) {
      fprintf(stderr, "error generating code for extern");
    }
  } else {
    getNextToken(); // skip current token
  }
}

static void HandleTopLevelExpression() {
  if (FunctionAST *F = ParseTopLevelExpr()) {
    if (!F->Codegen()) {
      fprintf(stderr, "error generating code for top level expr");
    }
  } else {
    getNextToken(); //  skip current token
  }
}

//== top ::= definition | external | expression | ';'
static void MainLoop() {
  while (1) {
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

//= Library functions

//== pushcard
extern "C"
double putchard(double X) {
  putchar((char)X);
  return 0;
}

//== printd
extern "C"
double printd(double X) {
  printf("%f\n", X);
  return 0;
}

int main() {
  initBinopPrecedence();

  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
  LLVMContext &Context = getGlobalContext();

  std::unique_ptr<Module> Owner = make_unique<Module>("", Context);
  TheModule = Owner.get();

  TheModule->addModuleFlag(Module::Warning, "Debug Info Version",
                           DEBUG_METADATA_VERSION);

  if (Triple(sys::getProcessTriple()).isOSDarwin())
    TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

  DBuilder = new DIBuilder(*TheModule);
  KSDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C,
                                                "fib.ks",
                                                ".",
                                                "Kaleidoscope Compiler",
                                                0,
                                                "",
                                                0);

  std::string ErrStr;
  TheExecutionEngine = EngineBuilder(std::move(Owner))
    .setErrorStr(&ErrStr)
    .setMCJITMemoryManager(llvm::make_unique<SectionMemoryManager>())
    .create();
  if (!TheExecutionEngine) {
    fprintf(stderr, "could not create ExecutionEngine: %s\n", ErrStr.c_str());
    exit(1);
  }

  FunctionPassManager OurFPM(TheModule);
  OurFPM.add(new DataLayoutPass());
#if 0
  OurFPM.add(createBasicAliasAnalysisPass());
  OurFPM.add(createPromoteMemoryToRegisterPass());
  OurFPM.add(createInstructionCombiningPass());
  OurFPM.add(createReassociatePass());
  OurFPM.add(createGVNPass());
  OurFPM.add(createCFGSimplificationPass());
#endif
  OurFPM.doInitialization();
  TheFPM = &OurFPM;

  getNextToken(); // Prime the first token

  MainLoop();

  TheFPM = 0;
  DBuilder->finalize();
  TheModule->dump();

  return 0;
}

