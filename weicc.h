#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node Node;
typedef struct Type Type;
typedef struct Member Member;

//
// strings.c
//

char *format(char *fmt, ...);


//
// tokenize.c
//

typedef enum {
  TK_IDENT,   // Identifiers
  TK_PUNCT,   // Punctuators
  TK_KEYWORD, // Keywords
  TK_STR,     // String literals
  TK_NUM,     // Numeric literals
  TK_EOF,     // End-of-file markers
} TokenKind;

// Token type
typedef struct Token Token;
struct Token {
  TokenKind kind; // Token kind
  Token *next;    // Next token

  // int, short, long -> their representation inside compiler are the same (int64)
  // only had difference in its code generation (mov 2/4/8 bytes into reg)
  int64_t val;        // If kind is TK_NUM, its value

  char *loc;      // Token location (name)
  int len;        // Token length
  Type *ty;       // Used if TK_STR
  char *str;      // String literal contents including terminating '\0'

  int line_no;    // Line number
};

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize_file(char *filename);

#define unreachable() \
  error("internal error at %s:%d", __FILE__, __LINE__)

//
// parse.c
//

// Variable or function
typedef struct Obj Obj;

struct Obj {
  Obj *next;
  char *name;    // Variable/Function name
  Type *ty;      // Variable Type
  bool is_local; // local or (global/function)

  // Local variable
  int offset;


  // Global variable or function
  bool is_function;

  // Global variable
  char *init_data;

  // Function
  Obj *params;
  Node *body;
  Obj *locals;
  int stack_size;
};

// AST Node
typedef enum {
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_ASSIGN,    // =
  ND_COMMA,     // ,
  ND_MEMBER,    // . (struct member access)
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
  ND_STMT_EXPR, // Statement Expression
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

  // Struct member access
  Member *member;

  char *funcname;
  Node *args;    
  Obj *var;      // Used if kind == ND_VAR

  int64_t val;       // Used if kind == ND_NUM

  // If statement
  Node *cond;
  Node *then;
  Node *els;

  // For statement
  Node *init;
  Node *inc;
};

Obj *parse(Token *tok);


//
// type.c
//

typedef enum {
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
  TY_STRUCT,
  TY_UNION,
} TypeKind;

struct Type {
  TypeKind kind;

  int size;  // sizeof() return value
  int align; // for sizeof() (specifically, struct sizeof)

  // Pointer-to or array-to type. We intentionally use the same
  // member to reprersent pointer / array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer of not. That means in many contexts "array of T" is 
  // naturally handled as if it were "pointer to T", as required by
  // the C spec

  Type *base;  // Pointer

  Token *name; // Declaration

  // Array
  int array_len;

  Member *members;

  Type *return_ty;
  Type *params;
  Type *next;
};


// Struct member
struct Member {
  Member *next;
  Type *ty;
  Token *name;
  int offset;
};

// extern: declare a global variable without any memory assigned to it
// so that we can access variables across C files
extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

bool is_integer(Type *ty);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int size);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);
