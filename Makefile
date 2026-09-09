# dart2cljd task runner (verification is local; no CI)
DART ?= /home/Schola/Projects/develop/flutter/bin/dart
CORPUS = corpus

.PHONY: test golden verify lint clean

test:
	clojure -M:test

golden:
	clojure -M -m dart2cljd.golden regen

verify:
	clojure -M -m dart2cljd.verify

lint:
	clojure -M:kondo

clean:
	rm -rf .cpcache target
