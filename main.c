#include "weicc.h"

static char *opt_o;

static char *input_path;

static void usage(int status) {
  fprintf(stderr, "weicc [ -o <path> ] <file>\n");
  exit(status);
}

static void parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--help"))
      usage(0);

    if (!strcmp(argv[i], "-o")) {
      if (!argv[++i])
	usage(1);
      opt_o = argv[i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      // strncmp only compare the first n chars (2 here)
      opt_o = argv[i] + 2;
      continue;
    }

    if (argv[i][0] == '-' && argv[i][1] != '\0') {
      error("unknown argument: %s", argv[i]);
    }

    // next time the file name
    input_path = argv[i];
  }

  if (!input_path)
    error("no input files");
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0)
  // By convention, read from stdin if a given filename is "-"
    return stdout;

  FILE *out = fopen(path, "w");

  if (!out)
    error("cannot open output file: %s: %s", path, strerror(errno));
  return out;
}


int main(int argc, char **argv) {
  parse_args(argc, argv);

  // Token *tok = tokenize(argv[1]);
  // Token *tok = tokenize_file(argv[1]);
  Token *tok = tokenize_file(input_path);

  Obj *prog = parse(tok);

  FILE *out = open_file(opt_o);
  fprintf(out, ".file 1 \"%s\"\n", input_path);
  codegen(prog, out);

  return 0;
}
