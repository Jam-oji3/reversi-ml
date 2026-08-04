export OCAMLMAKEFILE = ./OCamlMakefile

export LIBS=unix

export OCAMLFLAGS += -I +unix
export OCAMLLDFLAGS += -I +unix

define PROJ_client
	RESULT =reversi
	SOURCES=color.mli color.ml pos.mli pos.ml command.mli command.ml commandParser.mly commandLexer.mll bitboard.mli bitboard.ml eval.ml time_control.ml solve.ml search.ml strategy.mli strategy.ml play.mli play.ml main.ml
endef
export PROJ_client
define PROJ_server
	RESULT =reversi-serv
	SOURCES=color.mli color.ml pos.mli pos.ml command.mli command.ml commandParser.mly commandLexer.mll bitboard.mli bitboard.ml eval.ml time_control.ml solve.ml search.ml strategy.mli strategy.ml play.mli play.ml server.ml
endef
export PROJ_server

ifndef SUBPROJS
  export SUBPROJS = client server
endif

all: byte-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
	@rm -f commandLexer.ml commandParser.ml commandParser.mli
