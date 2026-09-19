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

// Generic mixin whose members do not mention the type parameter: the
// parameter is dropped (ClojureDart has no bounds support).
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

@Deprecated('use newName')
int oldName = 1;

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
