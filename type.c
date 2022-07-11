#include "weicc.h"


// This type system only used in type
// no relationship with the actual value

Type *ty_char = &(Type){TY_CHAR, 1, 1};
Type *ty_short = &(Type){TY_SHORT, 2, 2};
Type *ty_int = &(Type){TY_INT, 4, 4};
Type *ty_long = &(Type){TY_LONG, 8, 8};

static Type *new_type(TypeKind kind, int size, int align) {
  // allocate new heap memory for this type
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = kind;
  ty->size = size;
  ty->align = align;
  return ty;
}

bool is_integer(Type *ty) {
  TypeKind k = ty->kind;
  return k == TY_CHAR || k == TY_INT || k == TY_LONG
    || k == TY_SHORT;
}

Type *copy_type(Type *ty) {
  // copy a type object
  Type *ret = calloc(1, sizeof(Type));
  *ret = *ty;
  return ret;
}

Type *pointer_to(Type *base) {
  // 1. allocate new heap memory for this type
  Type *ty = new_type(TY_PTR, 8, 8);
  // 2. It pointes to the base address
  ty->base = base;
  return ty; // return its address as pointer
}

Type *array_of(Type *base, int len) {
  Type *ty = new_type(TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->array_len = len;

  return ty;
}

Type *func_type(Type *return_ty) {
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_FUNC;
  ty->return_ty = return_ty;
  return ty;
}

// type feature
// DFS
// add specific type to current node and
// its all child nodes
// for something like calculate size...
void add_type(Node *node) {
  if (!node || node->ty) {
    return;
  }

  // all possibilities (we do not know current type)
  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->cond);
  add_type(node->then);
  add_type(node->els);
  add_type(node->init);
  add_type(node->inc);

  for (Node *n = node->body; n; n = n->next) {
    add_type(n);
  }

  for (Node *n = node->args; n; n = n->next) {
    add_type(n);
  }

  switch (node->kind) {
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_NEG:
      node->ty = node->lhs->ty;
      return;
    case ND_ASSIGN:
      if (node->lhs->ty->kind == TY_ARRAY)
	error_tok(node->lhs->tok, "not an lvalue");
      // binary expressions
      node->ty = node->lhs->ty;
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_NUM:
    case ND_FUNCALL:
      node->ty = ty_long;
      return;
    case ND_VAR:
      node->ty = node->var->ty;
      return;
    case ND_COMMA:
      node->ty = node->rhs->ty;
      return;
    case ND_MEMBER:
      node->ty = node->member->ty;
      return;
    case ND_ADDR:
      if (node->lhs->ty->kind == TY_ARRAY)
	node->ty = pointer_to(node->lhs->ty->base);
      else
	node->ty = pointer_to(node->lhs->ty);
      return;
    case ND_DEREF:
      // key point
      if (!node->lhs->ty->base)
	error_tok(node->tok, "invalid pointer dereference");
      node->ty = node->lhs->ty->base;
      return;
    case ND_STMT_EXPR:
      if (node->body) {
	Node *stmt = node->body;
	while (stmt->next) {
	  stmt = stmt->next;
	}
	if (stmt->kind == ND_EXPR_STMT) {
	  // at parsing phase 
	  // (compound_stmt) is guarenteed to adding the type to each stmt/declaration
	  node->ty = stmt->lhs->ty;
	  return;
	}
      }
      error_tok(node->tok, "statement expression returning void is not supported");
      return;
    default:
      return;
  }
}
