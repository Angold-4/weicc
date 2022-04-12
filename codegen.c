#include "weicc.h"

static int depth;

static void push(void) {
  printf("  push %%rax\n");
  depth++;
}

static void pop(char *arg) {
  printf("  pop %s\n", arg);
  depth--;
}

// DFS, each iteration the value stored in the %rax
static void gen_expr(Node *node) {
  // unary / primary
  switch (node->kind) {
  case ND_NUM:
    printf("  mov $%d, %%rax\n", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    printf("  neg %%rax\n");
    return;
  default:
    break;
  }

  // binary expression
  // DFS
  gen_expr(node->rhs);
  push();      // push rax into stack (store)
  gen_expr(node->lhs); // store in rax
  pop("%rdi"); // pop rdi

  switch (node->kind) {
  case ND_ADD:
    printf("  add %%rdi, %%rax\n");
    return;
  case ND_SUB:
    printf("  sub %%rdi, %%rax\n");
    return;
  case ND_MUL:
    printf("  imul %%rdi, %%rax\n");
    return;
  case ND_DIV:
    printf("  cqo\n");
    // divide rax by rdi, store the quotient in rax
    printf("  idiv %%rdi\n");
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    printf("  cmp %%rdi, %%rax\n");

    if (node->kind == ND_EQ)
      printf("  sete %%al\n");
    else if (node->kind == ND_NE)
      printf("  setne %%al\n");
    else if (node->kind == ND_LT)
      printf("  setl %%al\n");
    else if (node->kind == ND_LE)
      printf("  setle %%al\n");

    printf("  movzb %%al, %%rax\n");
    return;
  default:
    error("invalid expression");
  }

  error("invalid expression");
}

static void gen_stmt(Node *node) {
  if (node->kind == ND_EXPR_STMT) {
    gen_expr(node->lhs);
    return;
  }
  error("invalid statement");
}

void codegen(Node *node) {
  printf(".section	__TEXT,__text,regular,pure_instructions\n");
  printf(".build_version macos, 12, 0	sdk_version 12, 0\n");
  printf(".globl _main\n");
  printf("_main:\n");

  for (Node *n = node; n; n = n->next) {
    // for each statement (stmt node)
    gen_stmt(n);
    assert(depth == 0);
  }

  assert(depth == 0);
  printf("  ret\n");
}
