.PHONY: all clean doc install uninstall reinstall

prefix = $(shell opam config var prefix)

all:
	pkg/pkg.ml build

clean:
	ocamlbuild -use-ocamlfind -clean

doc:
	topkg doc

install: all
	opam-installer --prefix="$(prefix)" adpkg.install

uninstall:
	opam-installer --prefix="$(prefix)" -u adpkg.install

reinstall: uninstall install
