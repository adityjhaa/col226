.DEFAULT_GOAL := run

run: tokenizer
	@ocamlrun tokenizer
	@rm tokenizer.cm*
	@rm tokenizer

tokenizer: tokenizer.ml
	@ocamlc tokenizer.ml -o tokenizer

