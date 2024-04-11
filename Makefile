EXEC=test.native

# Source files and directories to watch
SOURCES=$(wildcard src/*.ml) $(wildcard src/*.mll) $(wildcard src/*.mly)
SRC_DIR=src

# Source file to compile
SOURCE=test/test.mc

# Output file
OUTPUT=test/test.out

.PHONY: all clean run

all: $(EXEC)

$(EXEC): $(SOURCES) $(SOURCE)
	ocamlbuild -I $(SRC_DIR) $(EXEC)

run: $(EXEC)
	./$(EXEC) <$(SOURCE) >$(OUTPUT)

clean:
	ocamlbuild -clean
	rm -f $(OUTPUT)

