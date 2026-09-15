# dart2cljd

A **deterministic** Dart → [ClojureDart](https://github.com/Tensegritics/ClojureDart)
(`.cljd`) translator: pure rule-based, no AI, no heuristics. Same input and
flags always produce byte-identical output.

Inspired by [DartClojure](https://github.com/d00mch/DartClojure), but complete:
full language coverage, real source positions in diagnostics, and two explicit
modes — `--faithful` (1:1 structural mapping) and `--idiomatic` (adds Flutter
sugar: `f/widget` threading, `State` → atoms). Anything genuinely untranslatable
fails loudly with a `path:line:col` diagnostic; placeholder markers
(`:unknown`, `:unidiomatic`) never appear in output.

```
Dart source ─► bin/astdump.dart ─► EDN AST ─► normalize ─► emit ─► post-passes ─► printed .cljd
              (package:analyzer)              (IR records)  (rules)  (idiomatic sugar)
```

## Requirements

- Dart SDK 3.13+ on `PATH` (or `DART2CLJD_DART=/path/to/dart`) — runs the
  analyzer frontend.
- Java + Clojure CLI — runs the translator.
- For the compile gate only: a ClojureDart checkout at `../ClojureDart` (the
  `verify/` project points at it) and Flutter on `PATH`.

## Usage

```console
$ bin/dart2cljd "Text('hi')"                 # snippet (argument)
(m/Text "hi")

$ bin/dart2cljd -p lib/main.dart             # whole file (emits the ns form)
$ cat snippet.dart | bin/dart2cljd -         # stdin
$ bin/dart2cljd -i "Scaffold(body: Center(child: Text('hi')))"
(f/widget m/Scaffold .body m/Center (m/Text "hi"))
```

The CLI keeps DartClojure's flag surface, so editor integrations port over:

| Flag | Meaning |
|---|---|
| `-r true` | REPL mode: read snippets until the end marker (`-e TEXT`) |
| `-p PATH` | translate a Dart file (ns form included) |
| `-m ALIAS` | alias for `package:flutter/material.dart` (default `m`) |
| `-f ALIAS` | alias for `cljd.flutter` (default `f`) |
| `-c true` | colorize output |
| `-e TEXT` | REPL end marker (default: empty line) |
| `-i` / `--idiomatic` | idiomatic mode; `--faithful` is the default |
| `-w N` | printer line width (default 100) |
| `--default-alias A` | alias for unresolved Dart types |
| `--dart PATH` | Dart binary used by the frontend |

### API

```clojure
(require '[dart2cljd.api :as api])

(api/convert "Text('hi')" :format :sexpr)
;; => (m/Text "hi")

(api/convert "a % b")
;; => "(. a % b)"                 ; Dart euclidean %, rendered as the Dart operator

(api/convert "Foo(child: Bar(child: Baz(child: Qux())))" :mode :idiomatic)
;; => "(f/widget m/Foo m/Bar m/Baz (m/Qux))"

(api/convert-file "corpus/m3.dart")
;; => the whole translation, starting with (ns m3)
```

Accepted kwargs: `:format :string|:sexpr`, `:mode :faithful|:idiomatic`,
`:material`, `:flutter`, `:default-alias`, `:width`, `:ns`, `:dart`.

## Modes

**Faithful** maps Dart constructs 1:1, including classes (→ `deftype` with
constructor make-fns), mutation (→ `volatile!`/`atom`), loops, patterns,
switch, try/catch, enums, typedefs and annotations.

```dart
class Point {
  double x; double y;
  Point(this.x, this.y);
  Point operator +(Point o) => Point(x + o.x, y + o.y);
}
```

```clojure
(deftype Point
  [^:mutable ^double x ^:mutable ^double y]
  :type-only true
  Point
  (^Point + [this o] (Point-new (+ x (.-x o)) (+ y (.-y o)))))

(defn ^Point Point-new [^double x ^double y]
  (let [this (Point x y)] this))
```

**Idiomatic** adds opt-in Flutter sugar on top: `StatelessWidget` subclasses
become builder fns, `StatefulWidget`+`State` pairs become one fn with state
atoms subscribed through `f/widget :watch`, `.child`/`.body`/`.home` chains of
three or more widgets are threaded by `f/widget`, and `%` uses `mod`.
Unrecognized widget shapes fall back to the faithful `deftype` output.

```dart
class Greeting extends StatelessWidget {
  final String name;
  const Greeting({super.key, required this.name});
  @override
  Widget build(BuildContext context) =>
      Padding(padding: const EdgeInsets.all(8), child: Center(child: Text('Hello $name')));
}
```

```clojure
(defn Greeting [^String .name]
  (f/widget
    :context context
    (m/Padding .padding (m/EdgeInsets.all 8))
    m/Center
    (m/Text (str "Hello " name))))
```

## Verification

Three independent gates, all runnable locally:

```console
$ make test      # unit + golden tests (translation rules, byte-stable corpus)
$ make lint      # clj-kondo, 0 warnings
$ make verify    # the killer gate: real ClojureDart compiles + runs the corpus
$ make gate      # all three
```

`make verify` (`clojure -M:verify`) is the end-to-end proof:

1. translates every `corpus/<name>.dart` (mode from `corpus/manifest.edn`) into
   the `verify/` cljd Flutter project as `verify.gen.<name>`;
2. regenerates `verify.main` requiring all of them;
3. runs one `clj -M:cljd test verify.checks`: the **real ClojureDart compiler**
   compiles every generated namespace (aborting on compile errors) and
   `flutter test` executes the translated code, including the async path;
4. strips the bogus `test/cljd-out` imports that ClojureDart's test-namespace
   renaming leaves in generated `lib/` Dart (an upstream bug; the imports are
   unused) and runs `dart analyze` on every compiled Dart file, asserting zero
   issues.

The corpus is a pair per entry: `corpus/<name>.dart` plus the reviewed
`corpus/<name>.cljd`. `make golden` regenerates the expected files; `make
fixture` refreshes the cached astdump EDN used by the fast unit tests.

## Repository layout

```
bin/astdump.dart        Dart -> EDN syntax AST (package:analyzer, schema-versioned)
bin/dart2cljd           CLI wrapper
src/dart2cljd/
  ast.cljc              EDN AST -> normalized IR
  ir.cljc               IR schema table
  mutable.cljc          reassigned/captured-local analysis (R1)
  pattern.cljc          Dart 3 patterns -> cond tests + binds (R4)
  typeinfo.cljc         syntactic local-type pass for operator dispatch
  alias.cljc            import -> :require alias table (deterministic)
  emit/core.cljc        expressions, operators, calls, interop
  emit/statements.cljc  control flow, loops, switches, try/catch
  emit/decls.cljc       ns, classes/deftype, enums, typedefs, annotations
  idiomatic.cljc        widget classes -> fns, State -> atoms, f/widget
  printer.cljc          deterministic pretty-printer
  api.clj               convert/convert-file + DartClojure-compatible CLI
  verify.clj            compile/run verification harness
  golden.clj            golden corpus regeneration
corpus/                 .dart inputs + reviewed .cljd outputs + manifest
verify/                 cljd Flutter project used by the compile gate
test/                   unit + golden tests, fixtures, integration corpora
RESEARCH.md             compiler probes and the decisions they bind
```

## Documented divergences

These are deliberate and recorded with their probe evidence in
[`RESEARCH.md`](RESEARCH.md):

- `e as T` → `^T` hint; the Dart runtime `CastError` is not reproduced.
- Faithful `%` emits `(. a % b)` (Dart euclidean `%`); idiomatic mode uses `mod`.
- Generators (`yield`, `async*`, `sync*`) and `operator []=` fail loudly:
  ClojureDart cannot express them.
- Dart extensions and extension types fail loudly.
- Non-const records are wrapped in `cljd.core/identity` (upstream compiler bug).
- User-operator dispatch at call sites uses a syntactic local-type pass; types
  flowing through arbitrary expressions fall back to numeric operators.
- Idiomatic `key` constructor params are dropped; rebuilds are driven by state
  atom swaps, so a `setState` that writes no state does not rebuild.
- Mutability/type analyses are name-based and over-approximate shadowing
  (worst case: an unnecessary `volatile!` box).
