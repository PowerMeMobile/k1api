NAME=k1api
OTP_PLT=~/.otp.plt
PRJ_PLT=$(NAME).plt

all: generate

generate: compile xref
	@rm -rf ./rel/$(NAME)
	@./rebar generate

compile: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

update-deps:
	./rebar update-deps

console:
	./rel/$(NAME)/bin/$(NAME) console

develop:
	./rel/$(NAME)/bin/$(NAME) develop

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

xref:
	@./rebar skip_deps=true xref

api-test:
	@./rebar skip_deps=true eunit suites=k1api_common_test

mt-postpaid-test:
	@./rebar skip_deps=true eunit suites=k1api_common_test tests=outbound_sms_postpaid_test_

mt-prepaid-test:
	@./rebar skip_deps=true eunit suites=k1api_common_test tests=mt_prepaid_sms_test_

mo-test:
	@./rebar skip_deps=true eunit suites=k1api_common_test tests=incoming_sms_sub_test_