#include "weicc.h"


Type *ty_int = &(Type){TY_INT};

bool is_integer(Type *ty) {
  return ty->kind == TY_INT;
}

Type *pointer_to(Type *base) {
  // 1. allocate new heap memory for this type
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_PTR;
  // 2. It pointes to the base address
  ty->base = base;
  return ty; // return its address as pointer
}

// type feature
// add specific type to current node and
// its all child nodes
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

  switch (node->kind) {
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_NEG:
    case ND_ASSIGN:
      // binary expressions
      node->ty = node->lhs->ty;
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_NUM:
      node->ty = ty_int;
      return;
    case ND_VAR:
      node->ty = node->var->ty;
      return;
    case ND_ADDR:
      node->ty = pointer_to(node->lhs->ty);
      return;
    case ND_DEREF:
      // key point
      if (node->lhs->ty->kind != TY_PTR)
	error_tok(node->tok, "invalid pointer dereference");
      node->ty = node->lhs->ty->base;
      return;
    default:
      return;
  }
}
