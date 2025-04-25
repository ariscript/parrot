SNAKE_EXT=parr
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FLAGS=-mstackrealign -m64 -g -masm=intel -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer -z noexecstack
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho64
  CLANG_FLAGS=-mstackrealign -m64 -g -masm=intel -fstack-protector-all -Wstack-protector -Wno-format -fno-omit-frame-pointer -arch x86_64
endif
endif

PKGS=ounit2,extlib,unix,str,ppx_deriving,ppx_variants_conv,ppx_deriving.show,ppx_deriving.eq,ppx_deriving.ord
BUILD=ocamlbuild -r -use-ocamlfind -cflag -annot -ocamlyacc 'ocamlyacc -v'

.PHONY: test
test: test/*.ml lib/*.ml lib/parser.mly lib/lexer.mll exec/*.ml
	dune build
	dune exec ./test/test.exe

test/output/%.run: test/output/%.o exec/main.c exec/gc.c exec/snake.c exec/checks.c
	clang $(CLANG_FLAGS) -o $@ exec/snake.c exec/checks.c exec/gc.c exec/main.c $<

test/output/%.o: test/output/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: test/output/%.s
test/output/%.s: test/input/%.$(SNAKE_EXT)
	dune exec parrot $< > $@

test/output/do_pass/%.run: test/output/do_pass/%.o exec/main.c exec/gc.c exec/snake.c exec/checks.c
	clang $(CLANG_FLAGS) -o $@ exec/snake.c exec/checks.c exec/gc.c exec/main.c $<

.PRECIOUS: test/output/do_pass/%.s
test/output/do_pass/%.o: test/output/do_pass/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: test/output/do_pass/%.s
test/output/do_pass/%.s: test/input/do_pass/%.$(SNAKE_EXT)
	dune exec parrot $< > $@


test/output/dont_pass/%.run: test/output/dont_pass/%.o exec/main.c exec/gc.c exec/snake.c exec/checks.c
	clang -g $(CLANG_FLAGS) -o $@ exec/snake.c exec/checks.c exec/gc.c exec/main.c $<

test/output/dont_pass/%.o: test/output/dont_pass/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: test/output/dont_pass/%.s
test/output/dont_pass/%.s: test/input/dont_pass/%.$(SNAKE_EXT)
	dune exec parrot $< > $@


test/output/do_err/%.run: test/output/do_err/%.o exec/main.c exec/gc.c exec/snake.c exec/checks.c
	clang $(CLANG_FLAGS) -o $@ exec/snake.c exec/checks.c exec/gc.c exec/main.c $<

test/output/do_err/%.o: test/output/do_err/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: test/output/do_err/%.s
test/output/do_err/%.s: test/input/do_err/%.$(SNAKE_EXT) 
	dune exec parrot $< > $@


test/output/dont_err/%.run: test/output/dont_err/%.o exec/main.c exec/gc.c exec/snake.c exec/checks.c
	clang -g $(CLANG_FLAGS) -o $@ exec/snake.c exec/checks.c exec/gc.c exec/main.c $<

test/output/dont_err/%.o: test/output/dont_err/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: test/output/dont_err/%.s
test/output/dont_err/%.s: test/input/dont_err/%.$(SNAKE_EXT) 
	dune exec parrot $< > $@


test/output/exact/%.run: test/output/exact/%.o exec/main.c exec/gc.c exec/snake.c exec/checks.c
	clang $(CLANG_FLAGS) -o $@ exec/snake.c exec/checks.c exec/gc.c exec/main.c $<

test/output/exact/%.o: test/output/exact/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: test/output/exact/%.s
test/output/exact/%.s: test/input/exact/%.$(SNAKE_EXT) 
	dune exec parrot $< > $@


clean:
	rm -rf test/output/*.o test/output/*.s test/output/*.dSYM test/output/*.run *.log *.o
	rm -rf test/output/*/*.o test/output/*/*.s test/output/*/*.dSYM test/output/*/*.run
	rm -rf _build/
