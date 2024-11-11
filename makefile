init:
	opam switch create .
	opam install ocaml-lsp-server ocamlformat odoc

using_env:
	eval $(opam env)

build: using_env
	dune build

run: build
	dune exec -- bin/main.exe

lint: using_env
	dune fmt
	opam dune-lint
	opam lint

test: using_env
	dune test --release

docs: using_env build
	dune build @doc

clean:
	dune clean

destroy:
	opam switch remove .
