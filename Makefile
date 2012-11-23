NAME=k1api
OTP_PLT=~/.otp.plt
PRJ_PLT=$(NAME).plt

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

dialyze: $(OTP_PLT) compile $(PRJ_PLT)
	@dialyzer --plt $(PRJ_PLT) -r ./subapps/*/ebin

$(OTP_PLT):
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps erts \
		kernel stdlib crypto mnesia sasl common_test eunit ssl \
		asn1 compiler syntax_tools inets

$(PRJ_PLT):
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(PRJ_PLT) \
		-r ./deps/*/ebin ./subapps/*/ebin

compile-fast:
	@./rebar compile

generate-fast: compile-fast
	@rm -rf ./rel/$(NAME)
	@./rebar generate

api-test:
	@./rebar skip_deps=true eunit suites=k1api_common_test