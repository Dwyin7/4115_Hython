EXEC=test.native

# Source file to compile
SOURCE=test/test.mc

# Output file
OUTPUT=test/test.out

.PHONY: all clean run

all: $(EXEC)

$(EXEC):
	ocamlbuild -I src $(EXEC)

run: $(EXEC)
	./$(EXEC) <$(SOURCE) >$(OUTPUT)

clean:
	ocamlbuild -clean
	rm -f $(OUTPUT)
