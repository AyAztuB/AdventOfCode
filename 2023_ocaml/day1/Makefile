SRC=day1.ml
BIN=day1

all: $(BIN)

$(BIN): $(SRC)
	ocamlopt -o $@ $^

clean:
	$(RM) $(BIN) *.o *.cmi *.cmx

.PHONY: all clean
