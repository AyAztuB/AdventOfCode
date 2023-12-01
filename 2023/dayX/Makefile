CC=gcc
CPPFLAGS=-D_POSIX_C_SOURCE=200809L
CFLAGS=-Wall -Wextra -Werror -Wvla -pedantic -std=c99
LDFLAGS=
LDLIBS=

OBJ=day1.o
BIN=day1

all: $(BIN)
	./$(BIN)

$(BIN): $(OBJ)

check: CFLAGS += -g -fsanitize=address
check: LDFLAGS += -fsanitize=address
check: all

clean:
	$(RM) $(OBJ) $(BIN)

.PHONY: all check clean
