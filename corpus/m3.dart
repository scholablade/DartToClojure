// m3.dart — M3 faithful-emission corpus: classes, mutation, loops, patterns.

class Point {
  double x;
  double y;
  Point(this.x, this.y);
  Point.origin()
      : x = 0,
        y = 0;
  double get norm => x * x + y * y;
  set norm(double v) {
    x = v;
    y = v;
  }
  Point operator +(Point o) => Point(x + o.x, y + o.y);
  Point operator -() => Point(-x, -y);
  static Point zero = Point(0, 0);
  static Point make(double v) => Point(v, v);
  String toString() => 'Point($x, $y)';
}

class Base {
  String name;
  Base(this.name);
  String greet() => 'hi $name';
}

// M8: a constructor body assigns and increments its own fields (implicit
// `this`), which the generated make-fn must route through the instance.
class CtorBody {
  int count;

  CtorBody()
      : count = 0 {
    count = 3;
    count++;
  }
}

// M8: `late` fields are assigned after construction; the generated make-fn
// passes a nil placeholder and the field is emitted without a type tag (a
// nullable hint breaks non-nullable read sites, a non-nullable one rejects
// the placeholder).
class LateField {
  late int value;

  void setIt(int v) {
    value = v;
  }

  int readIt() => value;
}

// M8: `late final` fields are assigned once after construction (constructor
// bodies, initState); they need `^:mutable` in the generated deftype.
class LateFinal {
  late final int fromCtor;
  late final int fromMethod;

  LateFinal(int v) {
    fromCtor = v;
  }

  void init() {
    fromMethod = 7;
  }

  int sum() => fromCtor + fromMethod;
}

// M8: private Dart types are renamed (`_Foo` -> `d2c_Foo`): ClojureDart
// declares the class as-is but resolves references through the generated
// library's self-alias, and Dart rejects private names behind a prefix.
class _PrivateBox {
  int value;

  _PrivateBox(this.value);

  int doubled() => value * 2;
}

int privateBoxSum(int v) => _PrivateBox(v).doubled();

// M8: non-constant parameter defaults (collections, calls, ...) cannot be Dart
// parameter defaults; they become a nil default plus a body rebind.
class Defaults {
  final List<int> values;
  final String label;

  Defaults({this.values = const [1, 2], this.label = 'x'});

  int total() => values.length + label.length;
}

int defaultSum({List<int> xs = const [1, 2, 3], int add = 0}) =>
    xs.length + add;

// M8: zero-parameter closures in argument position are lifted to a local, so
// Dart does not infer them in a `void Function()` context (a value-returning
// closure literal there is rejected).
class VoidCallbackHost {
  int value = 0;

  void run(void Function() cb) {
    cb();
  }

  void bump() {
    run(() {
      value = value + 1;
    });
  }
}

// M8: Dart locals whose names collide with cljd syntax (`when`, `let`, `and`,
// `not`, ...) shadow the emitted forms (ClojureDart resolves locals before
// macros/special forms); they are renamed to `<name>$local`.
class ShadowHost {
  num run(int let, int when) {
    var and = let + when;
    and++;
    final not = and * 2;
    return not;
  }
}

// M8: `late` field initializers may reference `this` (`vsync: this`) or other
// late fields; the generated make-fn assigns them after the constructor body.
class LateInit {
  late final int start;
  late final int doubled = start * 2;
  late final LateInit self = this;

  LateInit(int v) {
    start = v;
  }
}

// M8: field reads/writes inside closures must go through `this`:
// ClojureDart snapshots field locals in closures and forbids `set!` on them.
class ClosureField {
  int value = 0;

  void Function() writer() {
    return () {
      value = 5;
    };
  }

  int Function() reader() {
    return () => value;
  }
}

class Child extends Base {
  int n;
  Child(this.n, String name) : super(name);
  @override
  String greet() => '${super.greet()}!';
}

// M8.18: a parameter that shadows a field is a real local (Dart scoping is
// innermost-first): `error(...)` calls the parameter, it does not read the
// field.
class ShadowField {
  final String error;

  ShadowField(this.error);

  String describe(String Function(String) error) => error(this.error);
}

// M8.18: a constructor parameter typed with the class's type variable must
// not be hinted (`^T` is not in scope in the namespace-level make-fn).
class Holder<T> {
  final T value;

  const Holder(this.value);

  Holder.named(T value) : this(value);
}

int holderLength() => Holder<List<int>>([1, 2, 3]).value.length;

// M8.18: non-scalar parameter hints are emitted only for overrides whose
// superclass chain leaves the unit. A same-file base declares the method
// unhinted (dynamic), so the override must stay dynamic too — Dart rejects an
// override whose parameter type differs from the overridden method's.
class Greeter {
  String greet(Object other) => 'hi';
}

class NarrowGreeter extends Greeter {
  @override
  String greet(covariant Greeter other) => 'hi ${other.runtimeType}';
}

String narrowGreet() => NarrowGreeter().greet(NarrowGreeter());

// M8.18: loop locals are hinted from their scalar initializer, so numeric
// member reads do not see an `as num` cast (`i.isEven`).
bool loopVarEven(int n) => [for (var i = 0; i < n; i++) i.isEven].first;

// M8.18: `~/` chains must not be flattened (`quot` takes two arguments).
int tripleDiv(int a, int b, int c) => a ~/ b ~/ c;

abstract class Shape {
  String label;
  Shape(this.label);
  double area();
  String describe() => '$label: $area';
}

class Square extends Shape {
  double side;
  Square(this.side, String label) : super(label);
  @override
  double area() => side * side;
}

enum Color { red, green, blue }

enum Suit {
  hearts(2),
  spades(11);

  final int value;
  const Suit(this.value);
}

// Named enum-constant arguments, including a declared `name` field that
// overrides the implicit enum member.
enum Rank {
  bronze(id: 'b', name: 'Bronze', weight: 1),
  gold(id: 'g', name: 'Gold', weight: 3);

  const Rank({required this.id, required this.name, required this.weight});
  final String id;
  final String name;
  final int weight;
}

// In-file static member shapes: bare field/getter/setter references and a
// qualified static setter write.
class Usage {
  static int hits = 0;

  static int get total => hits;

  static set total(int v) {
    hits = v;
  }

  static void record(int n) {
    hits += n;
  }

  static String summary() => '$hits/$total';
}

void setTotal(int v) {
  Usage.total = v;
}

typedef Cb = void Function(int);
typedef Pair<A, B> = (A, B);

int fib(int n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

// Untyped parameters: the receiver type is not statically known, so the
// translation must guard operator dispatch at runtime.
Point addAny(a, b) => a + b;

// M8: a mixin with fields is emitted as a Dart mixin (fields become `late`)
// and its members are inlined into every applier, which implements the mixin.
mixin Tagged {
  final String prefix = 'tag';
  bool _active = false;

  String get tag => '$prefix:$_active';

  void activate() {
    _active = true;
  }
}

class TaggedItem with Tagged {
  final String name;
  TaggedItem(this.name);
  String describe() => '$name ${tag}';
}

// Generic mixin whose members do not mention the type parameter: the mixin
// declaration keeps the parameter (`on Base<T>` needs it), appliers implement
// it raw; members that use the parameter fail loudly (no bounds support).
mixin Counted<T> {
  int _count = 0;
  int get count => _count;
  void bump() {
    _count++;
  }
}

class CountedBox with Counted<String> {
  final String label;
  CountedBox(this.label);
  String renderCount() => '$label=${count}';
}

// M8: collection elements must splice: C-style collection-for, conditional
// spreads and null-aware elements.
List<int> collect(int n, bool flag, List<int> extra, int? maybe) {
  return [
    for (var i = 0; i < n; i++) i * 2,
    if (flag) ...[100, 200] else ...[300],
    for (final x in extra) ...[x, x + 1],
    ?maybe,
  ];
}

// M8: a statement for without break/continue must recur, and multi-variable
// loops need per-variable updaters.
int countUp(int n) {
  var total = 0;
  for (var i = 0; i < n; i++) {
    total += i;
  }
  return total;
}

String multiVar(int n) {
  var out = '';
  for (var i = 0, j = n; i < j; i++, j--) {
    out = '$out$i-$j;';
  }
  return out;
}

void loops() {
  var sum = 0;
  for (var i = 0; i < 10; i++) {
    if (i == 3) continue;
    if (i == 8) break;
    sum += i;
  }
  while (sum > 0) {
    sum--;
  }
  do {
    sum += 2;
  } while (sum < 0);
  outer:
  for (final x in [1, 2, 3]) {
    for (final y in [1, 2]) {
      if (y == 1) continue outer;
      break outer;
    }
  }
  var list = [1, 2, 3];
  switch (list) {
    case [int a, ...var rest]:
      print(a);
      print(rest);
    case {'k': int v}:
      print(v);
    case (int x, String y) when x > 0:
      print(y);
    default:
      print('none');
  }
  final sw = switch (sum) {
    0 => 'zero',
    int n when n > 0 => 'pos',
    _ => 'neg',
  };
  print(sw);
  var (a, b) = (1, 2);
  (a, b) = (b, a);
  print('$a $b');
}

Future<int> work(int x) async {
  await Future.delayed(const Duration(seconds: 1));
  return x + 1;
}

// M8.18: a closure whose tail is a void call discards the value (Dart
// semantics); ClojureDart would otherwise `return` it (`void` in a
// `Future<Null>`/`void` closure context).
void runCallback(void Function() cb) => cb();

void schedule(int n, void Function(int) report) {
  runCallback(() {
    if (n > 0) {
      report(n);
    }
  });
}

// M8.18: a local initialized by `await` is bound dynamically: ClojureDart's
// `no-future` only unwraps a canonical `Future`, so a type that merely
// implements Future would be annotated with the pre-await type.
Future<int> delayedValue(int x) async => x + 1;

Future<int> awaitLocal(int x) async {
  final value = await delayedValue(x);
  return value + 1;
}

// M8.18: a nested closure has its own control flow; the enclosing function's
// early-return guards must not leak into it (the closure runs after the
// outer function returned, when the outer `d2c$done` flag is already set).
int guardedClosure(int n) {
  if (n < 0) return 0;
  final f = () => n * 2;
  return f();
}

// M10: Dart `String +`/`+=` concatenate. String locals, parameters and
// fields are tracked syntactically; `str` replaces the numeric `+` (which
// ClojureDart would compile to a numeric cast).
class StringHost {
  String label = 'a';
  int count = 0;

  StringHost(String start) {
    label += start;
  }

  void bump() {
    this.label += '!';
    label = label + '?';
    count += 1;
  }

  String describe() => label + ':' + count.toString();
}

String stringOps(String s, int n) {
  var out = s;
  out += '!';
  out = out + '?' + n.toString();
  out += n.isEven ? 'even' : 'odd';
  return out;
}

// M10: a local assigned inside a nested function is boxed as an atom; the
// write must be visible to the enclosing scope (`volatile!` would not be).
int closureMut(int n) {
  var total = 0;
  final bump = () {
    total += n;
  };
  bump();
  bump();
  return total + 1;
}

String closureString() {
  var out = 'x';
  final add = () {
    out += 'y';
  };
  add();
  return out;
}

@Deprecated('use newName')
int oldName = 1;

// M12: Dart built-in identifiers are legal field names but are reserved words
// for ClojureDart, which munges them to `$name_` in the generated Dart. Writes
// must use the munged spelling (`.-$base_!`) — its `.-base!` setter sugar has
// no munge fallback and compiled to a dynamic member assignment (RESEARCH
// M12.2). Covers ctor-body, method, closure and cascade writes.
class ReservedFields {
  int base;
  String tag;
  late int late;

  ReservedFields(int b, String t, int l) : base = b, tag = t {
    late = l;
    base = base + 1;
  }

  int readBase() => base;

  int bumpBase(int by) {
    base = base + by;
    return base;
  }

  int bumpInClosure(int by) {
    final bump = () {
      base += by;
    };
    bump();
    return base;
  }

  String readTag() => tag;

  void appendTag(String s) {
    tag = tag + s;
  }

  int readLate() => late;

  void setLate(int v) {
    late = v;
  }

  int bumpLate() {
    this.late++;
    return late;
  }

  void copyBase(ReservedFields other) {
    other.base = base + 1;
  }

  void cascade() {
    this
      ..base = 5
      ..late = 6;
  }
}

void main() {
  final p = Point(1, 2);
  p.x = 3;
  p.norm = 4;
  print(p + Point.make(1));
  print(Point.zero);
  print(Color.red);
  print(Suit.hearts.value);
  print(fib(5));
  loops();
  print(oldName);
  Usage.record(2);
  print(Usage.summary());
  setTotal(5);
  print(Usage.summary());
}
