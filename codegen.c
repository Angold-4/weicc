#include "weicc.h"

static int depth;

static int count(void) {
  static int i = 1; 
  // static var will only be initialized once
  return i++;
}

static void push(void) {
  printf("  push %%rax\n");
  depth++;
}

static void pop(char *arg) {
  printf("  pop %s\n", arg);
  depth--;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

// Compute the absolute address of a given node
// It's an error if a given node does not reside in memory
static void gen_addr(Node* node) {
  // temporary total 26 variables (a-z)
  if (node->kind == ND_VAR) {
    printf("  lea %d(%%rbp), %%rax\n", node->var->offset); // address (in stack)
    return;
  }
  error("not a lvalue"); // temp only variable is lvalue
}

// DFS, each iteration the value stored in the %rax
// every time this function wants to update %rax
// it need to store prev rax by pushing it into stack
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
    printf("  mov (%%rax), %%rax\n"); // move stack address into rax
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
  switch(node->kind) {
    case ND_IF: {
      int c = count();
      gen_expr(node->cond); // store result in %rax
      printf("  cmp $0, %%rax\n");
      printf("  je .L.else.%d\n", c);
      gen_stmt(node->then);
      printf("  jmp .L.end.%d\n", c);

      printf(".L.else.%d:\n", c);
      if (node->els) {
	gen_stmt(node->els);
      }
      printf(".L.end.%d:\n", c);
      return;
    }

    case ND_FOR: {
      int c = count();
      gen_stmt(node->init);
      printf(".L.begin.%d:\n", c);
      if (node->cond) {
	gen_expr(node->cond);
	printf("  cmp $0, %%rax\n");
	printf("  je .L.end.%d\n", c);
      }

      gen_stmt(node->then); // for each iteration
      
      if (node->inc) {
	gen_expr(node->inc);
      }

      printf("  jmp .L.begin.%d\n", c);
      printf(".L.end.%d:\n", c);
      return;
    }
    case ND_BLOCK:
      for (Node *n = node->body; n; n = n->next)
	gen_stmt(n);
      return;
    case ND_RETURN:
      // unary
      gen_expr(node->lhs); // result in rax
      printf("  jmp .L.return\n");
      return;
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      return;
    default:
      break;
  }
  error("invalid statement");
}


// Assign offsets to local variables
static void assign_lvar_offsets(Function *prog) {
  int offset = 0; // After parsing all local variables...
  for (Obj *var = prog->locals; var; var = var->next) {
    offset += 8;
    var->offset = -offset;
  }
  prog->stack_size = align_to(offset, 16);

}

// Block (expr linked list)
// -> Actual Code

void codegen(Function *prog) {
  assign_lvar_offsets(prog);

  printf(".section	__TEXT,__text,regular,pure_instructions\n");
  printf(".build_version macos, 12, 0	sdk_version 12, 0\n");
  printf(".globl _main\n");
  printf("_main:\n");


  // Prologue
  // 224 = 26 * 8 + (2 * 8), means total 26 single letter variables 
  // (two more for stack canaries) in MacOSX

  printf("  pushq %%rbp\n");
  printf("  mov %%rsp, %%rbp\n"); // current base
  printf("  sub $%d, %%rsp\n", prog->stack_size + 16);

  // The canaries (protect the stack)
  // please refer to this blog:
  // https://a4org.github.io/OSDI/Chapter/Chapter1/SROPAttack.html
  printf("  movq ___stack_chk_guard@GOTPCREL(%%rip), %%rax\n");
  printf("  movq  (%%rax), %%rax\n");
  printf("  movq  %%rax, -8(%%rbp)\n");

  // gen expression
  gen_stmt(prog->body); // must a block expression
  assert(depth == 0);
  
  printf(".L.return:\n");
  printf("  addq $%d, %%rsp\n", prog->stack_size + 16);
  printf("  popq %%rbp\n");

  printf("  ret\n");
}
