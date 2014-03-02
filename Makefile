
bin/main: main.rs lex.rs
	rustc main.rs -o bin/main

bin/tests: lex.rs lextest.rs
	rustc --test lextest.rs -o bin/tests

runtests: bin/tests
	./bin/tests
