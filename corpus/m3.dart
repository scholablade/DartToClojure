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

typedef Cb = void Function(int);
typedef Pair<A, B> = (A, B);

int fib(int n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

// Untyped parameters: the receiver type is not statically known, so the
// translation must guard operator dispatch at runtime.
Point addAny(a, b) => a + b;

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
}
