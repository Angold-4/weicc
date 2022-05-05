#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node Node;
typedef struct Type Type;

//
// tokenize.c
//

typedef enum {
  TK_IDENT,   // Identifiers
  TK_PUNCT,   // Punctuators
  TK_KEYWORD, // Keywords
  TK_NUM,     // Numeric literals
  TK_EOF,     // End-of-file markers
} TokenKind;

// Token type
typedef struct Token Token;
struct Token {
  TokenKind kind; // Token kind
  Token *next;    // Next token
  int val;        // If kind is TK_NUM, its value
  char *loc;      // Token location (name)
  int len;        // Token length
};

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize(char *input);

//
// parse.c
//

// Local variable
typedef struct Obj Obj;

struct Obj {
  Obj *next;
  char *name;  // Variable name
  Type *ty;    // Variable Type
  int offset;  // Offset from %rbp
};

// Function
typedef struct Function Function;

struct Function {
  Node *body;
  Obj *locals; // local variables
  int stack_size;
};

// AST Node
typedef enum {
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_ASSIGN,    // =
  ND_RETURN,    // "return"
  ND_ADDR,      // unary &
  ND_DEREF,     // unary *
  ND_IF,        // "if"
  ND_FOR,       // "for" or "while"
  ND_BLOCK,     // { ... }
  ND_FUNCALL,   // Function call
  ND_NEG,       // unary -
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_VAR,       // Variable
  ND_EXPR_STMT, // Expression Statement
  ND_NUM,       // Integer
  ND_ERR,       // Error
} NodeKind;

// AST node type
struct Node {
  NodeKind kind; // Node kind
  Node *next;    // Node node (multiple exprssions)
  Type *ty;      // Type, e.g. int or pointer to int
  Token *tok;    // Representative token

  Node *lhs;     // Left-hand side (unary used)
  Node *rhs;     // Right-hand side

  Node *body;    // Used if kind == ND_BLOCK
  char *funcname;
  Node *args;    
  Obj *var;      // Used if kind == ND_VAR
  int val;       // Used if kind == ND_NUM

  // If statement
  Node *cond;
  Node *then;
  Node *els;
  // For statement
  Node *init;
  Node *inc;
};

Function *parse(Token *tok);


//
// type.c
//

typedef enum {
  TY_INT,
  TY_PTR,
} TypeKind;

struct Type {
  TypeKind kind;

  Type *base;  // Pointer

  Token *name; // Declaration
};

extern Type *ty_int;

bool is_integer(Type *ty);
Type *pointer_to(Type *base);
void add_type(Node *node);


//
// codegen.c
//

void codegen(Function *prog);
