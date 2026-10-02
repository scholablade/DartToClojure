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
| `--exports-file PATH` | batch mode: write the external imports/exports Dart shim here |
| `--exports-uri URI` | batch mode: URI under which generated code requires that shim |
| `--types` | file/batch modes: use analyzer-resolved static types (see below) |

### Batch conversion (`--dir`)

```console
$ dart2cljd --dir lib --out src --ns-prefix my.app
dart2cljd: orphan_part.dart: dart2cljd: library "main.dart" does not declare this file as a part at 1:1
dart2cljd: converted 12, merged 3, skipped 2, failed 1
$ echo $?
1
```

One `.cljd` per **library**, preserving the tree structure
(`lib/foo/bar_baz.dart` → `src/foo/bar_baz.cljd`, namespace
`my.app.foo.bar-baz`; the mapping is the inverse of ClojureDart's
`ns-to-paths`). `*.g.dart`, `*.freezed.dart` and `*.mocks.dart` are skipped by
default. Failures never abort the run: every file is attempted, failures are
reported per file with `file:line:col`, and the CLI exits nonzero with a
`converted N, merged P, skipped M, failed K` summary.

**Libraries and parts**: a file with `part 'x.dart';` directives is a library;
the part's declarations are merged into the library's namespace before
emission (Dart parts share the library's imports and private scope), so
`syntax`/type/constructor references between the library and its parts work
unchanged and part files produce no `.cljd` of their own (they appear under
`merged P`; the merged output is byte-stable). Both `part of 'main.dart';` and
`part of library.name;` (with a matching `library library.name;`) are
supported. A part that is missing, excluded from discovery, or claims another
library fails its library loudly; a `part of` file that no converted library
lists fails on its own. In single-file mode (`-p lib/main.dart`) the parts are
read from disk and merged too; converting a `part of` file on its own fails
loudly. Because ClojureDart resolves types eagerly, a library *with parts* has
its declarations stably reordered so a type is emitted before its uses (a
part-free library keeps its Dart declaration order byte-for-byte).

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
`dart:*` and external packages stay string requires.

**Barrel files (`export`)**: importing a re-exporting file pulls in the
generated namespaces of its project exports, and — when it re-exports or
imports packages — one require of a generated **exports shim** under the
alias `exports`, through which otherwise-unresolved names are qualified
(`exports/BlocProvider`, `exports/AudioPlayer`, `exports/log`). The shim
re-exports every external URI the converted tree imports or exports, which is
how parse-only batch mode resolves names from unaliased `package:`/`dart:`
imports. Pass `--exports-file PATH --exports-uri URI` to write it (it must
live in the target project's Dart package, e.g. `lib/d2c_exports.dart` /
`package:my_app/d2c_exports.dart`); without the flags the default-alias
fallback is kept and files that strictly need the shim (barrels) fail with an
actionable message.

**Mixins with fields** are supported by *inlining*: a project mixin is
emitted as a Dart `mixin` (its fields become `late`) and every applier
(`class C with M`) inlines the mixin's fields and concrete methods and
implements it (`implements M`), which keeps `is` checks and interface
satisfaction. External mixins (`SingleTickerProviderStateMixin`, …) keep the
Dart `with` clause. Generic mixins drop their type parameters (ClojureDart
has no bounds support); members that reference a type parameter, static mixin
members, `super` calls in mixin methods and member name collisions fail
loudly.

### Resolved static types (`--types`)

File (`-p`) and batch (`--dir`) modes accept `--types`: the frontend dumps the
analyzer-resolved unit and every expression carries its static type, which the
emitter uses for decisions the syntactic passes cannot make (the type of a
call result, another object's field, a static field, a top-level getter, an
external class):

| Resolved type of the receiver | Emission |
|---|---|
| `String` (either operand of `+`) | `(str …)` concatenation, also for `+=` |
| a class (unit, project or external) | the operator interop call `(. recv + arg)` |
| `int`/`double`/`num` | plain numeric arithmetic, no runtime `dart/is?` guard |
| `dynamic`/unknown | the syntactic heuristics (guarded dispatch, numeric fallback) |

The same dump carries `:element`, *what a name refers to* (instance member vs
top-level value vs static vs type), which settles the remaining implicit-`this`
ambiguities: an instance member inherited from outside the converted tree wins
over a same-named cross-file top-level value (Dart innermost-first) — for
reads, writes and increments — and a top-level function/getter (e.g.
unaliased `dart:math`'s `max`/`pi`) routes through the imports/exports shim
instead of becoming `(.-max this …)`. The write side additionally records the
class declaring a written field, so ClojureDart's reserved-word field munging
(`base` → `$base_`) is applied to writes through other files' instances
(`item.base = 1`). The typed dump also resolves the tree's external function
typedefs, so the separate `--fn-typedefs` pass only runs for parse-only
conversions.

```console
$ dart2cljd --types -p lib/vec.dart
```

Resolution needs the source project to be resolvable (`dart pub get`); a file
that cannot be resolved warns and falls back to the syntactic passes (never
to garbage). `--types` requires a file: snippets cannot be resolved.

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
  library.cljc          Dart libraries: `part`/`part of` assembly (M9)
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
corpus/typedproj/       M12 typed directory fixture (element kinds, `--types`)
RESEARCH.md             compiler probes and the decisions they bind
```

## Documented divergences

These are deliberate and recorded with their probe evidence in
[`RESEARCH.md`](RESEARCH.md):

- `e as T` → `^T` hint; the Dart runtime `CastError` is not reproduced.
- Faithful `%` emits `(. a % b)` (Dart euclidean `%`); idiomatic mode uses `mod`.
- Generators (`yield`, `async*`, `sync*`), `operator []=` and Dart
  extensions/extension types fail loudly: ClojureDart cannot express them.
  `export` is supported through the generated exports shim (batch mode);
  `part`/`part of` are supported by merging the part's declarations into the
  library's namespace (part files produce no output); mixins with fields are
  supported by inlining (`implements` + inlined members), with generic type
  parameters dropped. A subclass whose *implicit* super call must run a
  same-unit superclass's field initializers is not supported (give the
  subclass an explicit `: super(value)` call); the ClojureDart compile fails
  loudly (RESEARCH "Open items" O3).
- `part`/`part of`: the part's declarations are emitted in the *library's*
  namespace (they share the library's imports and private scope, so this is
  the exact mapping; part files get no `.cljd`). A part that is missing or
  excluded from discovery fails its library loudly (an excluded part cannot
  be translated, so the library would be incomplete); a part claimed by two
  libraries, a part that declares its own parts, or a `part of` file whose
  library does not list it fails loudly too. Parts declared with the named
  form (`part of library.name;`) need the matching `library library.name;`
  declaration. Because ClojureDart resolves types eagerly, a library with
  parts has its declarations stably reordered by local type references (a
  part-free library keeps its Dart order byte-for-byte). Private names are not
  indexed for cross-file resolution (Dart privacy is library-scoped), which
  also removes spurious duplicate warnings between files that each declare the
  same `_helper`.
- `late` fields: `late T x;` is assigned after construction, but ClojureDart
  constructors require every field. Non-nullable scalars (`int`, `num`,
  `double`, `bool`, `String`) keep their declared type and get a zero-value
  placeholder (a read before assignment returns the zero value instead of
  throwing `LateInitializationError`); other non-nullable late fields are
  emitted dynamic with a nil placeholder; `late final` fields are `^:mutable`
  (single assignment is still enforced by the Dart input compiling).
  Initializers that reference `this` (`vsync: this`) are assigned through
  `this` after the constructor body, in declaration order — eager instead of
  Dart's lazy-on-first-read. See RESEARCH M8.9.
- `String +`/`+=` concatenate: String locals/parameters, implicit-this and
  idiomatic state fields, literals/interpolations and `toString`-style
  operands are tracked syntactically and emitted as `str` (RESEARCH M10.1).
  A receiver whose type is unknown syntactically (`dynamic`, an external
  field, a call result) keeps the numeric `+` and fails loudly at runtime if
  it is a String; same-unit String statics and top-level String values are
  not tracked. General operator semantics still depend on the syntactic type
  pass (RESEARCH R2/C2).
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
  target is a bare type parameter fails loudly at use sites; in parse-only
  mode instance members inherited from outside the converted tree are
  invisible to the syntactic pass, so a cross-file value with the same name
  wins over them (`--types` fixes this exactly for reads and writes, see
  above).
- External *function typedefs* (`VoidCallback`, `ValueChanged<T>`, ...) are
  emitted as `Function` hints. Typed runs resolve them per file as part of
  `--types` (the `:fn-typedefs` facts of the dump); parse-only runs use the
  separate `--fn-typedefs` frontend pass (the source project must be
  `pub get`-ed, otherwise conversion keeps the ordinary hint and the compile
  fails loudly); ClojureDart's analyzer helper cannot resolve function-typedef
  elements (RESEARCH M8.10/M12.4). Class hints (`^m/Widget`) are unaffected.
- Fields named after Dart reserved words/built-in identifiers (`base`,
  `late`, ...) are munged by ClojureDart (`$base_`); the emitter reproduces
  the munging for every `this`-routed write (constructor bodies, closures,
  `this.x = v`, `this.x++`, cascades, `late` initializers), for same-class
  locals, and — in typed mode — for fields of converted classes written
  through any receiver (`item.base = 1` from another file), using the
  resolved write element and its declaring class (RESEARCH M12.2/M12.3).
  External Dart classes' fields are never munged (their Dart name is not
  changed). In parse-only mode a write through a receiver of unknown type
  keeps the plain `.-x!` spelling; ClojureDart's setter path does not munge
  it, so such a write to a reserved-name field fails at runtime (loudly).
- The astdump frontend is run through a kernel snapshot cached in
  `.dart2cljd-cache/` (`dart run` re-JITs `package:analyzer` at ~40 s per
  call; the snapshot is ~1.3 s and is rebuilt when `bin/astdump.dart`
  changes).
- A local initialized by `await` is bound dynamically (`cljd.core/identity`
  wrapper): ClojureDart's `no-future` unwraps only a canonical `Future`, so a
  value that merely implements Future (Supabase's `PostgrestBuilder` chains)
  would otherwise be annotated with the pre-await type — an analyzer error and
  a runtime cast failure (RESEARCH M8.18).
- Non-scalar parameter hints are emitted only where they cannot break an
  override or a cross-file call: scalar hints everywhere; project/class/type
  variable hints only on `@override` methods whose local superclass chain
  agrees (a hint on a method becomes part of the emitted Dart signature, and
  ClojureDart cannot resolve a generated library's own class at a cross-file
  call site). Same-file overrides of unhinted local bases stay dynamic
  (RESEARCH M8.18).
- C-style loop variables get scalar hints (`for (var i = 0; …)` → `^int i`):
  ClojureDart types `loop` locals from the binding symbol, not the
  initializer, and unhinted locals become `dynamic` (numeric interop then
  inserts `as num` casts, which Dart promotes — breaking `i.isEven`).
- `~/` emits the Dart interop operator `(. a "~/" b)` instead of
  `cljd.core/quot`: `quot` is declared `^num` although Dart's `num.~/`
  returns `int`, so int contexts (`int f() => (a ~/ 2).clamp(0, n)`) failed.
- M8/M9/M12 batch features (barrel `export` support + exports shim over
  imports, mixin-with-fields inlining, `part`/`part of` library assembly,
  C-style collection-for and collection-element splicing, statement-for
  fixes, typed element-kind routing for reads and writes, reserved-word field
  munging) and the analyzer-triage fixes are covered by
  `make gate` fixtures (`corpus/m3.dart`, `test/batchproj/`,
  `corpus/typedproj/`, 44 runtime checks). The generated probe tree converts
  34/34, compiles, and `dart analyze` reports 3 residual errors — all the
  documented upstream ClojureDart `^super` cast in classes that apply an
  external mixin (RESEARCH M8.18).

## License

Copyright (C) 2026 Scholablade.

Licensed under the GNU Affero General Public License, version 3 or (at your
option) any later version. See [`LICENSE`](LICENSE) for the full text.

Why AGPL: `dart2cljd` is a build-time translator that ships as source, so the
copyleft surface is the tooling rather than a hosted service — AGPL still
covers the case where someone hosts a modified `dart2cljd` as a web
translation service without publishing their changes (AGPL §13).

Upstream projects are *not* relicensed by this: `DartClojure` (the
inspiration) stays Apache-2.0 and `ClojureDart` (the compilation target) stays
EPL-1.0. No upstream source is vendored here — `verify/` depends on a local
`ClojureDart` checkout via `:local/root` as a normal library dependency.
