#include "weicc.h"

static int depth;

// only support up to 6 arguments
static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

static Obj *current_fn;

static void gen_expr(Node* node);
static void gen_stmt(Node* node);

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
  printf("\n");
}

static int count(void) {
  static int i = 1; 
  // static var will only be initialized once
  return i++;
}

static void push(void) {
  println("  push %%rax");
  depth++;
}

static void pop(char *arg) {
  println("  pop %s", arg);
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

      if (node->var->is_local) {
	// local variable
	println("  lea %d(%%rbp), %%rax", node->var->offset); // address (in stack)
      } else {
	// global variable
	println("  lea %s(%%rip), %%rax", node->var->name);   // address in the TEXT segment
      }
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

// Load a value from where %rax is pointing to.
static void load(Type *ty) {
  if (ty->kind == TY_ARRAY) {
    // if it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    //
    // And this is where "array is automatically converted to a pointer 
    // to the first element of the array i C" occurs
    return;
  }

  if (ty->size == 1)
    println("  movsbq (%%rax), %%rax");
  else 
    println("  mov (%%rax), %%rax");
}

// Store %rax to an address that the stack top is pointing to.
static void store(Type *ty) {
  pop("%rdi");

  if (ty->size == 1)
    println("  mov %%al, (%%rdi)");
  else
    println("  mov %%rax, (%%rdi)");
}

// DFS, each iteration the value stored in the %rax
// every time this function wants to update %rax
// it need to store prev rax by pushing it into stack
static void gen_expr(Node *node) {
  // unary / primary
  switch (node->kind) {
  case ND_NUM:
    println("  mov $%d, %%rax", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    println("  neg %%rax");
    return;
  case ND_VAR:
    // the behaviour is just like *&
    gen_addr(node); // this gen will put the address of that var into %rax
    // printf("  mov (%%rax), %%rax\n"); // get the value store in the stack
    load(node->ty);
    return;
  case ND_DEREF:
    // two cases
    // #1. On the left (lvar):
    // (finally)will switch to ND_VAR and then lea the address into %rax
    // #2. On the right (rvar):
    // (finally)will switch to ND_ADDR and then lea the address into %rax
    gen_expr(node->lhs); // will be the address (gen_addr)
    // printf("  mov (%%rax), %%rax\n"); // get the value store in the stack
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs); // just return the addr (compare it with var)
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();

    gen_expr(node->rhs); // acutal value (r)
    // and store it into rax
  
    // pop("%rdi"); // previous lhs address (the tmp variable)
    // printf("  mov %%rax, (%%rdi)\n"); // only support integers

    store(node->ty);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next) {
      gen_stmt(n);
    }
    return;
  case ND_FUNCALL: {
    int nargs = 0;

    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_expr(arg); // put the value into %rax
      push();
      nargs++;
    }

    for (int i = nargs-1; i >= 0; i--) {
      pop(argreg64[i]);
    }

    println("  mov $0, %%rax");
    println("  call %s", node->funcname);
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
    println("  add %%rdi, %%rax");
    return;
  case ND_SUB:
    println("  sub %%rdi, %%rax");
    return;
  case ND_MUL:
    println("  imul %%rdi, %%rax");
    return;
  case ND_DIV:
    println("  cqo");
    // divide rax by rdi, store the quotient in rax
    println("  idiv %%rdi");
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    println("  cmp %%rdi, %%rax");

    if (node->kind == ND_EQ)
      println("  sete %%al");
    else if (node->kind == ND_NE)
      println("  setne %%al");
    else if (node->kind == ND_LT)
      println("  setl %%al");
    else if (node->kind == ND_LE)
      println("  setle %%al");

    println("  movzb %%al, %%rax");
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
      println("  cmp $0, %%rax");
      println("  je .L.else.%d", c);
      gen_stmt(node->then);
      println("  jmp .L.end.%d", c);

      println(".L.else.%d:", c);
      if (node->els) {
	gen_stmt(node->els);
      }
      println(".L.end.%d:", c);
      return;
    }

    case ND_FOR: {
      int c = count();
      if (node->init) 
	gen_stmt(node->init);
      println(".L.begin.%d:", c);
      if (node->cond) {
	gen_expr(node->cond);
	println("  cmp $0, %%rax");
	println("  je .L.end.%d", c);
      }

      gen_stmt(node->then); // for each iteration
      
      if (node->inc) {
	gen_expr(node->inc);
      }

      println("  jmp .L.begin.%d", c);
      println(".L.end.%d:", c);

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
      // printf("  jmp .L.return\n");
      println("  jmp .L.return.%s", current_fn->name);
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
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) continue; // global variables

    int offset = 0; // After parsing all local variables...
    for (Obj *var = fn->locals; var; var = var->next) {
      offset += var->ty->size;
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}


static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function)
      continue;

    println("  .data");
    println("  .globl %s", var->name);
    println("%s:", var->name);

    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++) {
	println("  .byte %d", var->init_data[i]); // ascii
      }
    } else {
      println("  .zero %d", var->ty->size);
    }
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) continue; // global variables
    println("  .globl %s", fn->name);
    println("  .text");
    println("%s:", fn->name);
    current_fn = fn;

    // Prologue
    println("  push %%rbp");
    println("  mov %%rsp, %%rbp");
    println("  sub $%d, %%rsp", fn->stack_size);

    // Save passed-by-register arguments to the stack
    // as the local varaibles
    int i = 0; 
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->ty->size == 1) {
	println("  mov %s, %d(%%rbp)", argreg8[i++], var->offset);
      } else {
	println("  mov %s, %d(%%rbp)", argreg64[i++], var->offset);
      }
    }

    // Emit code (body)
    gen_stmt(fn->body);
    assert(depth == 0);
    
    // Epilogue
    println(".L.return.%s:", fn->name);
    println("  mov %%rbp, %%rsp");
    println("  pop %%rbp");
    println("  ret");
  }
}

// Block (expr linked list)
// -> Actual Code

void codegen(Obj *prog) {
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
