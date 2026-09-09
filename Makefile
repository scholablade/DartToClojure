# dart2cljd task runner (verification is local; no CI)
DART ?= /home/Schola/Projects/develop/flutter/bin/dart
CORPUS = corpus

.PHONY: test golden fixture verify lint clean

test:
	clojure -M:test

golden:
	clojure -M -m dart2cljd.golden regen

fixture:
	$(DART) run bin/astdump.dart test/sample.dart > test/fixtures/sample.edn
	$(DART) run bin/astdump.dart test/surface.dart > test/fixtures/surface.edn
	$(DART) run bin/astdump.dart test/parity.dart > test/fixtures/parity.edn
	$(DART) run bin/astdump.dart test/statements.dart > test/fixtures/statements.edn

verify:
	clojure -M -m dart2cljd.verify

lint:
	clojure -M:kondo

clean:
	rm -rf .cpcache target
