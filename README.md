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

`bin/dart2cljd` works from any directory and can be symlinked onto `PATH`
(it resolves symlinks and puts the checkout on the classpath itself):

```console
$ ln -sf "$PWD/bin/dart2cljd" ~/.local/bin/dart2cljd   # once, from the checkout
$ cd ~/my_app
$ dart2cljd -p lib/main.dart                 # relative paths use your cwd
```

`DART2CLJD_ROOT` overrides the checkout location if you move things around;
`DART2CLJD_DART` overrides the Dart binary.

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
| `--dir DIR` | batch mode: translate a whole Dart source tree |
| `--out DIR` | batch mode: output directory (required with `--dir`) |
| `--ns-prefix P` | batch mode: prefix for generated namespaces |
| `--exclude GLOB` | batch mode: skip files matching GLOB (repeatable) |
| `--package NAME` | batch mode: Dart package name (default: nearest `pubspec.yaml`) |

### Batch conversion (`--dir`)

```console
$ dart2cljd --dir lib --out src --ns-prefix my.app
dart2cljd: orphan_part.dart: dart2cljd: `part of` files are not supported ...
dart2cljd: converted 12, skipped 2, failed 1
$ echo $?
1
```

One `.cljd` per converted input, preserving the tree structure
(`lib/foo/bar_baz.dart` → `src/foo/bar_baz.cljd`, namespace
`my.app.foo.bar-baz`; the mapping is the inverse of ClojureDart's
`ns-to-paths`). `*.g.dart`, `*.freezed.dart` and `*.mocks.dart` are skipped by
default. Failures never abort the run: every file is attempted, failures are
reported per file with `file:line:col`, and the CLI exits nonzero with a
`converted N, skipped M, failed K` summary.

Conversion is **two-pass**, so the generated tree is a connected project: a
project index maps files to namespaces, declared type names to their namespace,
classes to their constructors/statics, and top-level values to their kinds.
Relative and own-`package:` imports become symbol requires
(`[my.app.widgets.button :as button]`), cross-file type references resolve to
the generated namespace, cross-file constructor / static calls are rewritten
to the generated `Class-new` / `Class-member` names, top-level
functions/getters/vars are called through their generated namespace, and
cross-file typedefs expand to their target type. Writes work too: mutable
top-level vars (`set!`), static fields, setters and increments across files.
`dart:*` and external packages stay string requires. `part`, `part of`
and `export` fail loudly (ClojureDart cannot express them).

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
Constructor `key` params are kept (the builder fn takes `.key` and wraps in
`KeyedSubtree`), and every `setState` bumps a hidden tick atom, so callbacks
that write no state atom still rebuild. `initState`-created fields (and their
`dispose`) become `f/widget :managed` resources created once, with
`vsync: this` mapped to the `:vsync` directive. Unrecognized widget shapes
fall back to the faithful `deftype` output.

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
fixture` refreshes the cached astdump EDN used by the fast unit tests. A
manifest entry can also be `:kind :dir` (an M6 whole-tree fixture): its
`:expect-failures` list is asserted by the gate, so the failure policy is
verified too, and the generated tree is compiled and exercised at runtime
through its `:entry` namespace.

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
  frontend.clj          astdump runner (cached kernel snapshot)
  batch.clj             --dir conversion: discovery, index, import resolution
  api.clj               convert/convert-file/convert-dir + CLI
  verify.clj            compile/run verification harness
  golden.clj            golden corpus regeneration
corpus/                 .dart inputs + reviewed .cljd outputs + manifest
verify/                 cljd Flutter project used by the compile gate
test/                   unit + golden tests, fixtures, integration corpora
test/batchproj/         M6 directory fixture (relative/package imports, part file)
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
- User-operator dispatch uses a syntactic type pass plus `dart/is?` guards for
  unknown *identifier* receivers; compound assignments and operators on other
  expression kinds (call results, property accesses) still fall back to
  numeric/plain operators.
- Mutability/type analyses are name-based and over-approximate shadowing
  (worst case: an unnecessary `volatile!` box).
- Batch mode resolves cross-file types, typedefs, top-level
  functions/getters/setters/vars and static members (reads, calls, writes and
  increments). Duplicate type/value names across files are a warning with
  first-wins by sorted path. Remaining batch approximations: a typedef whose
  target is a bare type parameter fails loudly at use sites; instance members
  inherited from outside the converted tree are invisible to the syntactic
  pass, so a cross-file value with the same name wins over them.
- The astdump frontend is run through a kernel snapshot cached in
  `.dart2cljd-cache/` (`dart run` re-JITs `package:analyzer` at ~40 s per
  call; the snapshot is ~1.3 s and is rebuilt when `bin/astdump.dart`
  changes).
