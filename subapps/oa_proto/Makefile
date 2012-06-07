
all: test

test: compile
	erl -noshell -pa ebin/ deps/protobuffs/ebin/ \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

get-deps:
	@./rebar get-deps

compile: get-deps
	@./rebar compile

clean:
	@./rebar clean