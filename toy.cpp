#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>

// Lexer
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

int main() {
  fprintf(stdout, "ready> ");

  int t;
  do {
    t = gettok();
    printtok(t);
  } while (t != tok_eof);
}

