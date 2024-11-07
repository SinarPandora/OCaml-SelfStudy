init:
    opam switch create .
    opam install ocaml-lsp-server ocamlformat odoc

build:
    eval $(opam env)
    dune build

run:
    just build
    dune exec -- bin/main.exe

lint:
    eval $(opam env)
    dune fmt
    opam dune-lint
    opam lint

test:
    eval $(opam env)
    dune test --release

docs:
    eval $(opam env)
    dune build @doc

clean:
    dune clean

destroy:
    opam switch remove .
