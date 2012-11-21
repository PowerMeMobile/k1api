NAME=k1api
OTP_PLT=~/.otp.plt

all: generate

generate: compile
	@rm -rf ./rel/$(NAME)
	@./rebar generate

compile: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

console:
	./rel/$(NAME)/bin/$(NAME) console

clean:
	@./rebar clean

dialyze: compile
	@dialyzer --plt $(NAME).plt -r ./subapps/*/ebin

build-otp-plt:
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps erts \
		kernel stdlib crypto mnesia sasl common_test eunit ssl \
		asn1 compiler syntax_tools inets

build-project-plt:
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(NAME).plt \
		-r ./deps/*/ebin ./subapps/*/ebin

compile-fast:
	@./rebar compile

generate-fast: compile-fast
	@rm -rf ./rel/$(NAME)
	@./rebar generate