EXEC=test.native

# Source file to compile
SOURCE=test.mc

# Output file
OUTPUT=test.out

.PHONY: all clean run

all: $(EXEC)

$(EXEC):
	ocamlbuild $(EXEC)

run: $(EXEC)
	./$(EXEC) <$(SOURCE) >$(OUTPUT)

clean:
	ocamlbuild -clean
	rm -f $(OUTPUT)
