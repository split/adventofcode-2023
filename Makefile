SRC = $(wildcard *.hs)
INPUT = $(wildcard *.input)
EXAMPLE = $(wildcard *.example)
OUTPUT = $(INPUT:%.input=build/%.output)
ALL = $(SRC:%.hs=%) $(SRC:%.hs=%.input) $(OUTPUT) $(EXAMPLE:%.example=build/%.example.output)
FLAGS = -O2
YEAR = 2022

all: build $(ALL)

build:
	@mkdir -p $@

watch:
	@while true; do make -q -s || make -s; sleep 1; done

%.input:
	@[ "${AOC_COOKIE}" ] && curl -s -H "cookie: ${AOC_COOKIE}" https://adventofcode.com/$(YEAR)/day/$(shell echo $* | sed -r 's/.*day0?([0-9]+).*/\1/')/input > $@ || exit 0

build/%.output: % %.input
	@./$< < $*.input > $@ && cat $@

build/%.example.output: % %.example
	@grep -v Part $*.example | ./$< > $@
	@grep Part $*.example | diff -u - $@ && echo $* example is valid

%:: %.hs
	@ghc -outputdir build $(FLAGS) -o $@ $<

.PHONY: clean
clean:
	@rm -rf build