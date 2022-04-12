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

// Compute the absolute address of a given node
// It's an error if a given node does not reside in memory
static void gen_addr(Node* node) {
  // temporary total 26 variables (a-z)
  if (node->kind == ND_VAR) {
    int offset = (node->name - 'a' + 3) * 8;
    printf("  lea %d(%%rbp), %%rax\n", -offset); // address (in stack)
    return;
  }
  error("not a lvalue"); // temp only variable is lvalue
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
  case ND_VAR:
    gen_addr(node);
    printf("  mov (%%rax), %%rax\n"); // move heap value into rax
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();

    gen_expr(node->rhs); // acutal value (r)
    // store them into rax
  
    pop("%rdi"); // previous rhs address

    printf("  mov %%rax, (%%rdi)\n");
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


  // Prologue
  // 224 = 26 * 8 + (2 * 8), means total 26 single letter variables 
  // (two more for stack canaries) in MacOSX

  printf("  pushq %%rbp\n");
  printf("  mov %%rsp, %%rbp\n"); // current base
  printf("  sub $224, %%rsp\n");

  // The canaries (protect the stack)
  // please refer to this blog:
  // https://a4org.github.io/OSDI/Chapter/Chapter1/SROPAttack.html
  printf("  movq ___stack_chk_guard@GOTPCREL(%%rip), %%rax\n");
  printf("  movq  (%%rax), %%rax\n");
  printf("  movq  %%rax, -8(%%rbp)\n");

  // gen expression
  for (Node *n = node; n; n = n->next) {
    // for each statement (stmt node)
    gen_stmt(n);
    assert(depth == 0);
  }
  
  printf("  addq $224, %%rsp\n");
  printf("  popq %%rbp\n");

  assert(depth == 0);
  printf("  ret\n");
}
