CFLAGS=-std=c11 -g -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

weicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

$(OBJS): weicc.h

test: weicc
	./test.sh
	./test-driver.sh

clean:
	rm -f weicc *.o *~ tmp*

.PHONY: test clean
