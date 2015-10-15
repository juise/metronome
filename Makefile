.PHONY: all deps compile run test clean
.PHONY: build_plt dialyzer

REBAR=./rebar

DIALYZER_APPS = asn1 compiler crypto erts inets kernel public_key sasl ssl stdlib syntax_tools tools

all: $(REBAR) deps compile

deps:
		$(REBAR) get-deps

compile:
		$(REBAR) compile

run:
		erl -pa deps/*/ebin ebin -config sys.config -args_file vm.args -boot start_sasl -s sync -run metronome

test:
		$(REBAR) eunit skip_deps=true verbose=3
		$(REBAR) ct skip_deps=true verbose=3

clean:
		$(REBAR) clean
		rm -rf ./ebin
		rm -rf ./test/*.beam
		rm -rf ./erl_crash.dump

build_plt: clean deps compile
ifneq ("$(wildcard erlang.plt)","")
		@echo "Erlang plt file already exists"
else
		dialyzer --build_plt --output_plt erlang.plt --apps $(DIALYZER_APPS)
endif
ifneq ("$(wildcard metronome.plt)","")
		@echo "metronome plt file already exists"
else
		dialyzer --build_plt --output_plt metronome.plt ebin/ deps/*/ebin/
endif

add_to_plt: build_plt
		dialyzer --add_to_plt --plt erlang.plt --output_plt erlang.plt.new --apps $(DIALYZER_APPS)
		dialyzer --add_to_plt --plt metronome.plt --output_plt metronome.plt.new ebin/ deps/*/ebin/
		mv erlang.plt.new erlang.plt
		mv metronome.plt.new metronome.plt

dialyzer:
		dialyzer --src src --plts erlang.plt metronome.plt -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs | fgrep -v -f ./dialyzer.ignore-warnings

