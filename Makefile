# dart2cljd task runner (verification is local; no CI)
DART ?= /home/Schola/Projects/develop/flutter/bin/dart
CORPUS = corpus

.PHONY: test golden fixture verify gate lint clean

test:
	clojure -M:test

golden:
	clojure -M -m dart2cljd.golden regen

fixture:
	$(DART) run bin/astdump.dart corpus/m3.dart > test/fixtures/m3.edn
	$(DART) run bin/astdump.dart corpus/idiomatic.dart > test/fixtures/idiomatic.edn
	$(DART) run bin/astdump.dart --types corpus/typed.dart > test/fixtures/typed.edn
	$(DART) run bin/astdump.dart test/sample.dart > test/fixtures/sample.edn
	$(DART) run bin/astdump.dart test/surface.dart > test/fixtures/surface.edn
	$(DART) run bin/astdump.dart test/parity.dart > test/fixtures/parity.edn
	$(DART) run bin/astdump.dart test/statements.dart > test/fixtures/statements.edn

verify:
	clojure -M:verify

gate: test lint verify

lint:
	clojure -M:kondo

clean:
	rm -rf .cpcache target .dart2cljd-cache
	rm -rf verify/.cpcache verify/.clojuredart verify/.dart_tool verify/lib \
	       verify/test/cljd-out verify/src/verify/gen verify/src/verify/main.cljd
