#include "weicc.h"

static int depth;

// only support up to 6 arguments
static char *argreg[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

static void gen_expr(Node* node);

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
  switch(node->kind) {
    case ND_VAR:
      // lea -- Load effective address
      // The lea instruction places the address specified by
      // its first operand into the register specified by its second operand.
      printf("  lea %d(%%rbp), %%rax\n", node->var->offset); // address (in stack)
      return;
    case ND_DEREF:
      // multiple dereference
      gen_expr(node->lhs);
      // stop until it reach a variable
      return;
    default:
      break;
  }

  error_tok(node->tok, "not a lvalue"); // temp only variable is lvalue
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
    // the behaviour is just like *&
    gen_addr(node); // this gen will put the address of that var into %rax
    printf("  mov (%%rax), %%rax\n"); // get the value store in the stack
    return;
  case ND_DEREF:
    // two cases
    // #1. On the left (lvar):
    // (finally)will switch to ND_VAR and then lea the address into %rax
    // #2. On the right (rvar):
    // (finally)will switch to ND_ADDR and then lea the address into %rax
    gen_expr(node->lhs); // will be the address (gen_addr)
    printf("  mov (%%rax), %%rax\n"); // get the value store in the stack
    return;
  case ND_ADDR:
    gen_addr(node->lhs); // just return the addr (compare it with var)
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();

    gen_expr(node->rhs); // acutal value (r)
    // and store it into rax
  
    pop("%rdi"); // previous lhs address (the tmp variable)

    printf("  mov %%rax, (%%rdi)\n"); // only support integers
    return;
  case ND_FUNCALL: {
    int nargs = 0;

    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_expr(arg); // put the value into %rax
      push();
      nargs++;
    }

    for (int i = nargs-1; i >= 0; i--) {
      pop(argreg[i]);
    }

    printf("  mov $0, %%rax\n");
    printf("  call %s\n", node->funcname);
    return;
  }
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

  error_tok(node->tok, "invalid expression");
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
      if (node->init) 
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
      // block -> declrations / stmts
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
  error_tok(node->tok, "invalid statement");
}


// Assign offsets to local variables
static void assign_lvar_offsets(Function *prog) {
  int offset = 8; // After parsing all local variables...
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

  printf(".globl main\n");
  printf("main:\n");

  printf("  push %%rbp\n");
  printf("  mov %%rsp, %%rbp\n"); // current base
  printf("  sub $%d, %%rsp\n", prog->stack_size);

  // gen expression
  gen_stmt(prog->body); // must a block expression
  assert(depth == 0);
  
  printf(".L.return:\n");
  printf("  add $%d, %%rsp\n", prog->stack_size);
  printf("  pop %%rbp\n");

  printf("  ret\n");
}
