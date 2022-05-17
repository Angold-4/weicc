#include "weicc.h"

// Takes a printf-style format string and returns a formatted string
char *format(char *fmt, ...) {
  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  va_list ap;
  va_start(ap, fmt); 
  // get the args in ap after fmt
  // guess a linked-list like stuff with a end indicator (\0)
  vfprintf(out, fmt, ap);
  va_end(ap); // After the call va_end(ap) the variable ap is undefined
  fclose(out);

  return buf;
}
