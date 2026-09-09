// sample.dart — exercises the language surface for astdump
library my.app;

import 'dart:async';
import 'package:flutter/material.dart' as m;
import 'package:collection/collection.dart' show groupBy;

@Deprecated('use bar')
const kMax = 0xFF;

typedef IntCallback = void Function(int x, {String? name});
typedef Pair<A, B> = (A, B);

enum Color { red, green, blue }

enum Suit with Comparable<Suit> implements Comparable<Suit> {
  hearts(2),
  spades(11);

  final int value;
  const Suit(this.value);
}

mixin Walker on Animal {
  void walk() => print('walk');
}

class Animal {
  String name;
  static const int legs = 4;
  int? _age;
  Animal(this.name, [this._age]);
  Animal.named(this.name, {int age = 0});
  factory Animal.fromName(String n) => Animal(n);
  const Animal.constant(this.name);
  Animal._private(this.name);

  String get label => '$name ($_age)';
  set label(String v) {
    name = v;
  }

  static Future<int> asyncWork(int x) async {
    await Future.delayed(const Duration(seconds: 1));
    return x + 1;
  }

  @override
  String toString() => 'Animal($name)';
}

abstract class Shape {
  double area();
  void describe() {}
}

class Circle extends Shape implements Comparable<Circle> {
  final double r;
  Circle(this.r) : assert(r > 0, 'r must be positive'), super();

  @override
  double area() => 3.14 * r * r;

  @override
  int compareTo(Circle other) => r.compareTo(other.r);
}

class Point<T extends num> {
  T x, y;
  Point(this.x, this.y);
  Point<T> operator +(Point<T> o) => Point(x + o.x, y + o.y);
  bool operator ==(Object other) => other is Point && x == other.x;
  Point<T> operator -() => Point(-x, -y);
}

class Counter extends StatefulWidget {
  const Counter({super.key, required this.start, this.label = 'count'});
  final int start;
  final String label;

  @override
  State<Counter> createState() => _CounterState();
}

class _CounterState extends State<Counter> {
  int _count = 0;

  void _increment() {
    setState(() {
      _count++;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Text('$label $_count', style: Theme.of(context).textTheme.displayLarge),
        for (var i = 0; i < 3; i++)
          if (i.isEven) const SizedBox(height: 8),
        ...?extras,
        FloatingActionButton(
          onPressed: _increment,
          child: const Icon(Icons.add),
        ),
      ],
    );
  }
}

extension NumberX on int {
  int get doubled => this * 2;
  int tripling() => this * 3;
}

extension type Meters(int value) implements int {
  Meters get km => Meters(value * 1000);
}

int? lateVar;
late final String greeting = 'hello';

void topLevel() {}

int add(int a, int b) => a + b;

Future<void> main(List<String> args) async {
  var x = 1;
  var y = 2.5;
  x++;
  x += 2;
  y ~/= 1;
  final list = [1, 2, ...x, ...?maybe, if (x > 0) 3, for (var i in [1, 2]) i];
  final m1 = {'a': 1, 'b': null};
  final s1 = {1, 2, 3};
  final rec = (1, 'two', named: true);
  final isBig = x > 10 && y < 3.0 || !(x == 1) || x != 2;
  final check = x is int && x is! double;
  final cast = x as num;
  final nul = x ?? 3;
  final cond = x > 0 ? 'pos' : 'neg';
  final neg = -x;
  final mod = x % 2;
  final div = x ~/ 2;
  final hex = 0xFF + 1_000 + .5;
  final cascade = Circle(1.0)
    ..area()
    ..r = 2.0;
  final nn = list[0];
  nn!;

  label:
  for (var i = 0; i < 10; i++) {
    if (i == 3) break label;
    if (i == 2) continue;
    print(i);
  }

  for (final e in list) {
    print(e);
  }

  while (x > 0) {
    x--;
    do {
      x += 0;
    } while (false);
  }

  try {
    throw StateError('boom');
  } on StateError catch (e, st) {
    rethrow;
  } on Exception {
    print('caught');
  } catch (e) {
    print(e);
  } finally {
    print('done');
  }

  switch (x) {
    case 1:
      print('one');
      break;
    case 2:
    case 3:
      print('2 or 3');
      break;
    default:
      print('other');
  }

  switch (rec) {
    case (int a, String b, named: bool c) when a > 0:
      print(a);
    case Circle(r: > 1.0):
      print(r);
    case [_, ...rest]:
      print(rest);
    case {'a': int v}:
      print(v);
    case String s || int s:
      print(s);
    default:
      print('none');
  }

  final sw = switch (x) {
    1 => 'one',
    int n when n > 0 => 'pos',
    _ => 'other',
  };

  final (a2, b2) = rec;
  var (c2, d2) = (1, 2);
  (a2, b2) = (b2, a2);

  Future<int> fib(int n) async {
    if (n < 2) return n;
    return await fib(n - 1) + fib(n - 2);
  }

  final f = (int a, [int b = 1]) => a + b;
  final g = ({int a = 1, required int b}) => a + b;
  int h(int a, {int b = 2}) => a + b;

  Stream<int> gen() async* {
    yield 1;
    yield* gen();
  }

  final closure = () {
    return () => x;
  };

  await fib(10);
  unawaited(asyncWork(1));

  if (x case int n when n > 5) {
    print(n);
  }

  assert(x > 0, 'x must be positive');
  return;
}
