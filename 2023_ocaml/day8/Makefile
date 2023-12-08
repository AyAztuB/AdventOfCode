SRC=$(shell find -name "*.ml")
BIN=$(SRC:.ml=)

all: $(BIN)
	$(BIN)

$(BIN): $(SRC)
	ocamlopt -o $@ $^

clean:
	$(RM) $(BIN) *.o *.cmi *.cmx

.PHONY: all clean
