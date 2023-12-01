CC=gcc
CPPFLAGS=-D_POSIX_C_SOURCE=200809L
CFLAGS=-Wall -Wextra -Werror -Wvla -pedantic -std=c99
LDFLAGS=
LDLIBS=

SRC=$(shell find -name "*.c")
OBJ=$(SRC:.c=.o)
BIN=$(OBJ:.o=)

all: $(BIN)
	$(BIN)

$(BIN): $(OBJ)

check: CFLAGS += -g -fsanitize=address
check: LDFLAGS += -fsanitize=address
check: all

clean:
	$(RM) $(OBJ) $(BIN)

.PHONY: all check clean
