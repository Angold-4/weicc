// This file contains a recursive decent parser for C
//
// Most functions in this file are named after symbols they are
// supposed to read from an input token list. For example, stmt()
// is responsible for reading a statementt form a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens, Since C doesn't support
// multiple return values, the remaining tokens are returned to
// the call via a point argument (especially **rest)
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this parser.

#include "weicc.h"

// Scope:
//
// <---- Inner -----> Outer
// SC1 -> SC2 -> SC3 -> ... -> SCn
// var1   var2   var3          varn

// Scope for local or global variables
typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next;
  char *name;
  Obj *var;
};

// Represents a block scope
typedef struct Scope Scope;
struct Scope {
  Scope *next;
  VarScope *vars;
};


// All local variable instances created during parsing are
// accumulated to this list.
static Obj *locals;
static Obj *globals;

static Scope *scope = &(Scope){};

static Type *declspec(Token **rest, Token *tok);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Node *funcall(Token **rest, Token *tok);


static void enter_scope(void) {
  Scope *sc = calloc(1, sizeof(Scope));
  sc->next = scope;
  scope = sc; // add to the beginning
}

static void leave_scope(void) {
  // outer scope
  scope = scope->next;
}

// Find a local variable by name
static Obj *find_var(Token *tok) {

  for (Scope *sc = scope; sc; sc = sc->next) {
    // for each scope (from inner to outer) (enter_scope)
    for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next) {
      if (equal(tok, sc2->name))
	return sc2->var;
    }
  }

  return NULL;
}

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = expr;
  return node;
}

static Node *new_num(int val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
  Node *node = new_node(ND_VAR, tok);
  node->var = var;
  return node;
}

static VarScope *push_scope(char *name, Obj *var) {
  VarScope *sc = calloc(1, sizeof(VarScope));
  sc->name = name;
  sc->var = var;
  sc->next = scope->vars;
  scope->vars = sc;
  return sc;
}

static Obj *new_var(char *name, Type* ty) {
  // helper function
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name; // name = .L..%id (in string)
  var->ty = ty;
  push_scope(name, var); // VarScope
  return var;
}

static Obj *new_lvar(char *name, Type *ty) {
  // new local variable
  Obj *var = new_var(name, ty);
  var->is_local = true;
  // -> * -> * -> * -> ... -> *
  var->next = locals;
  locals = var; // update the local variable linked list 
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  // -> * -> * -> * -> ... -> *
  var->next = globals;
  // append it to the beginning
  // note that the new_gvar will make all function reversal
  globals = var; // update the global variable linked list
  return var;
}

static char *new_unique_name(void) {
  static int id = 0; // static

  // char *buf = calloc(1, 20);
  // sprintf(buf, ".L..%d", id++);
  // sprintf() write to the character string str.

  return format(".L..%d", id++);
}

static Obj *new_anon_gvar(Type *ty) {
  return new_gvar(new_unique_name(), ty);
}

static Obj *new_string_literal(char *p, Type *ty) {
  Obj *var = new_anon_gvar(ty);
  var->init_data = p;
  return var;
}

static char *get_ident(Token *tok) {
  // a new variable
  // initially, its name stored in the TEXT segment (prorgam)
  // since the program will finally run on the machine
  // we need to allocate some area in the heap
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len); // return char* 
  // return the pointer in the heap (that store the string name)
  // strndup allocate memory for string in heap
}

static int get_number(Token *tok) {
  if (tok->kind != TK_NUM)
    error_tok(tok, "expected a number");
  return tok->val;
}


// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) of p.
// In other words, we need to scale an integer value before adding to 
// a pointer value. This function takes care of scaling.
static Node *new_add(Node* lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num + num
  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  if (lhs->ty->base && rhs->ty->base) {
    error_tok(tok, "invalid operands");
  }

  // Canonicalize `num + ptr` to `ptr + num`.
  if (!lhs->ty->base && rhs->ty->base) {
    // swap
    Node *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  // ptr + num
  // Now only support int (only int type supported currently)
  rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok); 
  return new_binary(ND_ADD, lhs, rhs, tok);
}

// Just like the `+`, `-` is overloaded for the pointer type.
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  // ptr - num
  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
    add_type(rhs);
    Node *node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = lhs->ty;
    return node;
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base) {
    Node *node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = ty_int;
    return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
  }

  error_tok(tok, "invalid operands");
}

// declspec = "int" | "char" | struct-decl
static Type *declspec(Token **rest, Token *tok) {
  if (equal(tok, "char")) {
    *rest = tok->next;
    return ty_char;
  }

  if (equal(tok, "int")) {
    *rest = tok->next;
    return ty_int;
  }

  if (equal(tok, "struct")) {
    return struct_decl(rest, tok->next);
  }

  error_tok(tok, "typename expected");
}

// func-params = (param ("," param)*)? " )"
// param = declspec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
  Type head = {}; // parameters
  Type *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }

    // declaration
    Type *basety = declspec(&tok, tok);       // int
    Type *ty = declarator(&tok, tok, basety); // idenitfier
    // allocate a new copied type object in heap
    cur = cur->next = copy_type(ty);        
  }

  ty = func_type(ty); // it is an function
  ty->params = head.next;
  *rest = tok->next; // update the pointer

  return ty;
}


// type_suffix = "(" func-params
// 	       | "[" num "]" type-suffix
// 	       | ε
// func-params = param ("," param)*
// param       = declspec declarator
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  // function
  if (equal(tok, "(")) {
    return func_params(rest, tok->next, ty);
  }

  // array
  if (equal(tok, "[")) {
    int sz = get_number(tok->next);
    tok = skip(tok->next->next, "]");
    ty = type_suffix(rest, tok, ty); // array of array
    return array_of(ty, sz); 
    // not actually assign a space for this array object
    // the actual array value at runtime is stored in the stack
  }

  // just ty
  *rest = tok;
  return ty;
}

// declarator = "*"* (0 or n) ident type-suffix
static Type *declarator(Token **rest, Token *tok, Type *ty) {
  while (consume(&tok, tok, "*")) {
    ty = pointer_to(ty); // create a pointer
  }

  if (tok->kind != TK_IDENT) {
    error_tok(tok, "expected a variable name");
  }

  ty = type_suffix(rest, tok->next, ty); 
  // get the type of this identifier
  // a function, array, or just a variable

  ty->name = tok;
  return ty;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok) {
  Type *basety = declspec(&tok, tok); // type (e.g, "int")

  // int x, y, z;  
  // (x, y, z) -> declarator ("=" expr)?

  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!equal(tok, ";")) {
    if (i++ > 0) {
      // the first declarator does not have ","
      tok = skip(tok, ",");
    }

    Type *ty = declarator(&tok, tok, basety);
    Obj *var = new_lvar(get_ident(ty->name), ty);

    if (!equal(tok, "=")) {
      // just a declaration
      continue;
    }

    // we have an initial value for this variable
    Node *lhs = new_var_node(var, ty->name);
    Node *rhs = assign(&tok, tok->next); // value
    Node *node = new_binary(ND_ASSIGN, lhs, rhs, tok);
    cur = cur->next = new_unary(ND_EXPR_STMT, node, tok);
    // sequential relationship
  }

  Node *node = new_node(ND_BLOCK, tok);
  node->body = head.next;
  *rest = tok->next;
  return node;
}

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
  return equal(tok, "char") || equal(tok, "int") || equal(tok, "struct");
}


// compound_stmt = (declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_BLOCK, tok);

  Node head = {};
  Node *cur = &head; // linked list

  enter_scope(); // create a new scope (sc)
  // and set scope = sc

  while (!equal(tok, "}")) {
    // try to generate stmt nodes
    if (is_typename(tok)) {
      cur = cur->next = declaration(&tok, tok);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }
    add_type(cur); // each stmt / declaration
  }

  leave_scope(); // outer scope

  node->body = head.next; // only block
  *rest = tok->next; // jump "}"

  return node;
}

// avoid left recursion
// stmt = "return" expr ";"
// 	| "if" "(" expr ")" stmt ("else" stmt)?
// 	| "for" "(" expr_stmt expr? ";" expr? ")" stmt
// 	| "while" "(" expr ")" stmt
// 	| "{" compound_stmt
//      | expr_stmt
static Node *stmt(Token **rest, Token *tok) {
  if (equal(tok, "return")) {
    Node *node = new_node(ND_RETURN, tok);
    node->lhs = expr(&tok, tok->next);
    *rest = skip(tok, ";");
    return node;
  }

  if (equal(tok, "if")) {
    Node *node = new_node(ND_IF, tok);

    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    node->then = stmt(&tok, tok);

    if (equal(tok, "else")) {
      // won't update the token
      node->els = stmt(&tok, tok->next);
    }

    *rest = tok;
    return node;
  }

  if (equal(tok, "for")) {
    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");

    node->init = expr_stmt(&tok, tok); // only one statement

    if (!equal(tok, ";")) {
      // we have condition
      node->cond = expr(&tok, tok);
    }
    tok = skip(tok, ";");

    if (!equal(tok, ")")) {
      // we have increment
      node->inc = expr(&tok, tok);
    }

    tok = skip(tok, ")");

    node->then = stmt(rest, tok);

    return node;
  }

  if (equal(tok, "while")) {
    // syntatic sugar
    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    node->then = stmt(rest, tok);
    return node;
  }

  if (equal(tok, "{")) {
    return compound_stmt(rest, tok->next);
  }

  return expr_stmt(rest, tok);
}

// expr_stmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok) {
  if (equal(tok, ";")) {
    // null statement
    *rest = tok->next;
    return new_node(ND_BLOCK, tok);
  }
  Node *node = new_node(ND_EXPR_STMT, tok);
  node->lhs = expr(&tok, tok);
  *rest = skip(tok, ";"); // ensure the current token is ';'
  return node;
}

// expr = assign ("," expr)? 
static Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);

  if (equal(tok, ","))
    // the comma operator
    // (1, 2, 3) ->return 3
    return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);

  *rest = tok;
  return node;
}

// assign = equality ("=" assign)?
static Node *assign(Token **rest, Token *tok) {
  // chain assign
  Node *node = equality(&tok, tok); // will reach the number
  if (equal(tok, "=")) { // consume an "="
    node = new_binary(ND_ASSIGN, node, assign(&tok, tok->next), tok);
  }
  *rest = tok;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "==")) {
      node = new_binary(ND_EQ, node, relational(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "!=")) {
      node = new_binary(ND_NE, node, relational(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
static Node *relational(Token **rest, Token *tok) {
  Node *node = add(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "<")) {
      node = new_binary(ND_LT, node, add(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "<=")) {
      node = new_binary(ND_LE, node, add(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">")) {
      node = new_binary(ND_LT, add(&tok, tok->next), node, start);
      continue;
    }

    if (equal(tok, ">=")) {
      node = new_binary(ND_LE, add(&tok, tok->next), node, start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    // nested tree
    Token *start = tok;
    if (equal(tok, "+")) {
      node = new_add(node, mul(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "-")) {
      node = new_sub(node, mul(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = unary ("*" unary | "/" unary)*
static Node *mul(Token **rest, Token *tok) {
  Node *node = unary(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, unary(&tok, tok->next), tok);
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, unary(&tok, tok->next), tok);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// unary = ("+" | "-" | "*" | "&") unary
//       | postfix
static Node *unary(Token **rest, Token *tok) {
  if (equal(tok, "+"))
    return unary(rest, tok->next);

  if (equal(tok, "-"))
    return new_unary(ND_NEG, unary(rest, tok->next), tok);

  if (equal(tok, "&"))
    return new_unary(ND_ADDR, unary(rest, tok->next), tok);

  if (equal(tok, "*"))
    // dereference
    return new_unary(ND_DEREF, unary(rest, tok->next), tok);

  return postfix(rest, tok);
}

// struct-members = (declspec declarator ("," declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {};
  Member *cur = &head;

  while (!equal(tok, "}")) {
    Type *basety = declspec(&tok, tok);
    int i = 0;

    while (!consume(&tok, tok, ";")) {
      if (i++)
	tok = skip(tok, ",");

      Member *mem = calloc(1, sizeof(Member));
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      cur = cur->next = mem;
    }
  }

  *rest = tok->next;
  ty->members = head.next;
}

// struct-decl = "{" struct-members
static Type *struct_decl(Token **rest, Token *tok) {
  tok = skip(tok, "{");

  // Construct a struct object
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_STRUCT;
  struct_members(rest, tok, ty);
  ty->align = 1;

  // Assign offsets within the struct to members
  int offset = 0;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    offset = align_to(offset, mem->ty->align);
    mem->offset = offset;
    offset += mem->ty->size;

    if (ty->align < mem->ty->align)
      // set as the maximum offset (align)
      ty->align = mem->ty->align;
  }

  ty->size = align_to(offset, ty->align);

  return ty;
}

static Member *get_struct_member(Type *ty, Token *tok) {
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (mem->name->len == tok->len &&
	!strncmp(mem->name->loc, tok->loc, tok->len)) {
      return mem;
    }
  }

  error_tok(tok, "no such member");
}

static Node *struct_ref(Node *lhs, Token *tok) {
  add_type(lhs);
  if (lhs->ty->kind != TY_STRUCT)
    error_tok(lhs->tok, "not a struct");

  Node *node = new_unary(ND_MEMBER, lhs, tok);
  node->member = get_struct_member(lhs->ty, tok);
  return node;
}


// postfix = primary ("[" expr "]" | "." ident)*
static Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  for (;;) {
    if (equal(tok, "[")) {
      // syntax sugar
      // x[y] == *(x+y)
      Token *start = tok;
      Node *idx = expr(&tok, tok->next);
      tok = skip(tok, "]");
      node = new_unary(ND_DEREF, new_add(node, idx, start), start);
      continue;
    }

    if (equal(tok, ".")) {
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    *rest = tok;
    return node;
  }
}

// primary = "(" "{" stmt+ "}" ")"
// 	   | "(" expr ")"
// 	   | "sizeof" unary
// 	   | ident func-args?
// 	   | str
// 	   | num
// func-args = "(" (assign ("," assign)*)? ")"
static Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(") && equal(tok->next, "{")) {
    // This is a GNU statement expression
    Node *node = new_node(ND_STMT_EXPR, tok);
    node->body = compound_stmt(&tok, tok->next->next)->body;
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "(")) {
    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "sizeof")) {
    Node *node = unary(rest, tok->next);
    add_type(node); // add type ! we want its size
    return new_num(node->ty->size, tok);
  }

  if (tok->kind == TK_IDENT) {
    // Function call
    if (equal(tok->next, "("))
      return funcall(rest, tok);

    // Variable
    Obj *var = find_var(tok);
    if (!var) {
      error_tok(tok, "undefined variable");
    }
    *rest = tok->next;
    return new_var_node(var, tok);
  }

  if (tok->kind == TK_STR) {
    Obj *var = new_string_literal(tok->str, tok->ty);
    *rest = tok->next;
    return new_var_node(var, tok);
  }

  if (tok->kind == TK_NUM) {
    Node *node = new_num(tok->val, tok);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
  return new_node(ND_ERR, tok); // never reach here
}

static void create_param_lvars(Type *param) {
  if (param) {
    create_param_lvars(param->next);
    // create a new variable and put them into locals
    new_lvar(get_ident(param->name), param);
  }
}

// funcall = identifier "(" (assign ("," assign)*)? ")"
static Node *funcall(Token **rest, Token *tok) {
  // make that function call
  Token *start = tok;    // current at identifier (record)
  tok = tok->next->next; // jump '('

  Node head = {};
  Node *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      // except the first time (no ',')
      tok = skip(tok, ",");
    }
    cur = cur->next = assign(&tok, tok);
  }

  *rest = skip(tok, ")");

  // build the node
  // do not check whether it is a illegal funcall
  Node *node = new_node(ND_FUNCALL, start);
  node->funcname = strndup(start->loc, start->len);
  node->args = head.next; // linked list
  return node;
}

static Token *function(Token *tok, Type *basety) {
  Type *ty = declarator(&tok, tok, basety);  

  Obj *fn = new_gvar(get_ident(ty->name), ty); // key:
  // already append that function into globals list
  fn->is_function = true;

  locals = NULL; // key:
  // each function will set the locals equal to NULL
  // then update its variable in that locals
  // finally store it in the fn->locals

  enter_scope();

  create_param_lvars(ty->params); // will update locals
  fn->params = locals;

  tok = skip(tok, "{"); // current program must inside "{" and "}"
  fn->body = compound_stmt(&tok, tok); // also parse locals
  fn->locals = locals; // notice that it will treat params as locals
  
  leave_scope();

  return tok; // updated token list
}

static Token *global_variable(Token *tok, Type *basety) {
  bool first = true;

  while (!consume(&tok, tok, ";")) {
    if (!first) 
      tok = skip(tok, ",");
    first = false;

    Type *ty = declarator(&tok, tok, basety);
    new_gvar(get_ident(ty->name), ty); // append to the globals
  }

  return tok;
}

// Lookahead tokens and returns true if the given token is a start
// of a function definition or declaration
static bool is_function(Token *tok) {
  if (equal(tok, ";"))
    return false;

  Type dummy = {};
  Type *ty = declarator(&tok, tok, &dummy);
  return ty->kind == TY_FUNC;
}


// Token Linked List
// -> expr

// program(Token list) = (function-definition | global-variable)*
Obj *parse(Token *tok) {
  globals = NULL;

  while (tok->kind != TK_EOF) {
    Type *basety = declspec(&tok, tok);

    // Function
    if (is_function(tok)) {
      tok = function(tok, basety);
      continue;
    }

    // Global variable
    tok = global_variable(tok, basety);
  }
  
  return globals;
}
